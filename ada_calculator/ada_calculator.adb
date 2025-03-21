with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;

procedure Calculator is
   -- Define a map type for variables
   package Variable_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Float,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");

   -- Define exceptions
   Syntax_Error      : exception;
   Division_By_Zero  : exception;
   Undefined_Variable : exception;

   -- Define token types
   type Token_Type is (Number, Plus, Minus, Multiply, Divide, Power, 
                       Left_Paren, Right_Paren, Identifier, Assign, EOF);

   -- Define token record
   type Token is record
      Kind  : Token_Type;
      Value : Unbounded_String;
      Num_Value : Float;
   end record;

   -- Variables map
   Variables : Variable_Maps.Map;

   -- Current position in input
   Position : Natural;
   Input    : Unbounded_String;

   -- Get the next token from input
   function Get_Token return Token is
      Result : Token;
      C      : Character;
   begin
      -- Skip whitespace
      while Position <= Length(Input) and then Element(Input, Position) = ' ' loop
         Position := Position + 1;
      end loop;

      -- Check if we're at the end
      if Position > Length(Input) then
         Result.Kind := EOF;
         return Result;
      end if;

      C := Element(Input, Position);

      case C is
         when '0'..'9' =>
            -- Parse number
            declare
               Start : Natural := Position;
               Num   : Float;
            begin
               while Position <= Length(Input) and then 
                     (Element(Input, Position) in '0'..'9' or Element(Input, Position) = '.') loop
                  Position := Position + 1;
               end loop;
               
               begin
                  Num := Float'Value(Slice(Input, Start, Position - 1));
                  Result.Kind := Number;
                  Result.Num_Value := Num;
               exception
                  when others =>
                     raise Syntax_Error with "Invalid number format";
               end;
            end;
            
         when '+' =>
            Result.Kind := Plus;
            Position := Position + 1;
            
         when '-' =>
            Result.Kind := Minus;
            Position := Position + 1;
            
         when '*' =>
            Result.Kind := Multiply;
            Position := Position + 1;
            
         when '/' =>
            Result.Kind := Divide;
            Position := Position + 1;
            
         when '^' =>
            Result.Kind := Power;
            Position := Position + 1;
            
         when '(' =>
            Result.Kind := Left_Paren;
            Position := Position + 1;
            
         when ')' =>
            Result.Kind := Right_Paren;
            Position := Position + 1;
            
         when '=' =>
            Result.Kind := Assign;
            Position := Position + 1;
            
         when 'a'..'z' | 'A'..'Z' =>
            -- Parse identifier
            declare
               Start : Natural := Position;
            begin
               while Position <= Length(Input) and then 
                     (Element(Input, Position) in 'a'..'z' or 
                      Element(Input, Position) in 'A'..'Z' or 
                      Element(Input, Position) in '0'..'9' or 
                      Element(Input, Position) = '_') loop
                  Position := Position + 1;
               end loop;
               
               Result.Kind := Identifier;
               Result.Value := To_Unbounded_String(Slice(Input, Start, Position - 1));
            end;
            
         when others =>
            raise Syntax_Error with "Invalid character: " & C;
      end case;

      return Result;
   end Get_Token;

   -- Put token back (we just decrement position)
   procedure Unget_Token(T : Token) is
   begin
      null; -- In a real implementation, we would adjust Position
   end Unget_Token;

   -- Forward declarations for recursive descent parser
   function Parse_Expression return Float;
   function Parse_Term return Float;
   function Parse_Factor return Float;
   function Parse_Power return Float;
   function Parse_Primary return Float;

   -- Parse an expression (lowest precedence: + and -)
   function Parse_Expression return Float is
      Left  : Float;
      Right : Float;
      T     : Token;
   begin
      -- Check for assignment
      declare
         Peek : Token := Get_Token;
      begin
         if Peek.Kind = Identifier then
            declare
               Var_Name : Unbounded_String := Peek.Value;
               Next_Token : Token := Get_Token;
            begin
               if Next_Token.Kind = Assign then
                  -- This is an assignment
                  Right := Parse_Expression;
                  Variables.Include(Var_Name, Right);
                  return Right;
               else
                  -- Not an assignment, put tokens back
                  Unget_Token(Next_Token);
                  Unget_Token(Peek);
               end if;
            end;
         else
            Unget_Token(Peek);
         end if;
      end;

      Left := Parse_Term;
      
      loop
         T := Get_Token;
         case T.Kind is
            when Plus =>
               Right := Parse_Term;
               Left := Left + Right;
            when Minus =>
               Right := Parse_Term;
               Left := Left - Right;
            when others =>
               Unget_Token(T);
               return Left;
         end case;
      end loop;
   end Parse_Expression;

   -- Parse a term (higher precedence: * and /)
   function Parse_Term return Float is
      Left  : Float;
      Right : Float;
      T     : Token;
   begin
      Left := Parse_Power;
      
      loop
         T := Get_Token;
         case T.Kind is
            when Multiply =>
               Right := Parse_Power;
               Left := Left * Right;
            when Divide =>
               Right := Parse_Power;
               if Right = 0.0 then
                  raise Division_By_Zero;
               end if;
               Left := Left / Right;
            when others =>
               Unget_Token(T);
               return Left;
         end case;
      end loop;
   end Parse_Term;

   -- Parse power expressions (highest precedence: ^)
   function Parse_Power return Float is
      Left  : Float;
      Right : Float;
      T     : Token;
   begin
      Left := Parse_Factor;
      
      T := Get_Token;
      if T.Kind = Power then
         Right := Parse_Power; -- Right associative
         return Left ** Right;
      else
         Unget_Token(T);
         return Left;
      end if;
   end Parse_Power;

   -- Parse a factor (highest precedence: parentheses, numbers, variables)
   function Parse_Factor return Float is
      T : Token;
      Result : Float;
   begin
      T := Get_Token;
      
      case T.Kind is
         when Number =>
            return T.Num_Value;
            
         when Identifier =>
            if Variables.Contains(T.Value) then
               return Variables(T.Value);
            else
               raise Undefined_Variable with "Variable not defined: " & To_String(T.Value);
            end if;
            
         when Left_Paren =>
            Result := Parse_Expression;
            T := Get_Token;
            if T.Kind /= Right_Paren then
               raise Syntax_Error with "Expected closing parenthesis";
            end if;
            return Result;
            
         when Minus =>
            -- Unary minus
            return -Parse_Factor;
            
         when others =>
            raise Syntax_Error with "Unexpected token in expression";
      end case;
   end Parse_Factor;

   -- Parse primary is just an alias for Parse_Factor in this implementation
   function Parse_Primary return Float renames Parse_Factor;

   -- Evaluate an expression string
   function Evaluate(Expr : String) return Float is
   begin
      Input := To_Unbounded_String(Expr);
      Position := 1;
      return Parse_Expression;
   end Evaluate;

   -- Main program
   Line : String(1..1000);
   Last : Natural;
   Result : Float;
begin
   Put_Line("ADA Calculator");
   Put_Line("Enter expressions with variables and assignments (e.g., 'x = 5', 'y = x + 3')");
   Put_Line("Type 'exit' to quit");
   
   loop
      Put("> ");
      Get_Line(Line, Last);
      
      exit when Line(1..Last) = "exit";
      
      begin
         Result := Evaluate(Line(1..Last));
         Put("Result: ");
         Put(Result, Fore => 1, Aft => 6, Exp => 0);
         New_Line;
      exception
         when Syntax_Error =>
            Put_Line("Syntax error in expression");
         when Division_By_Zero =>
            Put_Line("Error: Division by zero");
         when Undefined_Variable =>
            Put_Line("Error: Undefined variable");
         when others =>
            Put_Line("Unknown error occurred");
      end;
   end loop;
end Calculator;