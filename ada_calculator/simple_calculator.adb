with Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;

procedure Simple_Calculator is
   use Ada.Text_IO;
   use Ada.Float_Text_IO;
   use Ada.Strings.Unbounded;
   
   -- Variables map
   package Variable_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Unbounded_String,
      Element_Type => Float,
      Hash => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");
   
   Variables : Variable_Maps.Map;
   
   -- Exceptions
   Syntax_Error : exception;
   Undefined_Variable : exception;
   Division_By_Zero : exception;
   
   -- Tokenize and evaluate the expression
   function Evaluate (Expression : String) return Float is
      I : Positive := Expression'First;
      
      -- Skip whitespace
      procedure Skip_Whitespace is
      begin
         while I <= Expression'Last and then Expression(I) = ' ' loop
            I := I + 1;
         end loop;
      end Skip_Whitespace;
      
      -- Forward declarations
      function Parse_Expression return Float;
      function Parse_Term return Float;
      function Parse_Factor return Float;
      function Parse_Primary return Float;
      
      -- Parse a primary expression (number, variable, or parenthesized expression)
      function Parse_Primary return Float is
         Result : Float;
         Start : Positive;
      begin
         Skip_Whitespace;
         
         if I > Expression'Last then
            raise Syntax_Error with "Unexpected end of input";
         end if;
         
         -- Parse number
         if Expression(I) in '0'..'9' then
            Start := I;
            while I <= Expression'Last and then 
                  (Expression(I) in '0'..'9' or Expression(I) = '.') loop
               I := I + 1;
            end loop;
            
            return Float'Value(Expression(Start..I-1));
            
         -- Parse variable
         elsif Expression(I) in 'a'..'z' or Expression(I) in 'A'..'Z' then
            Start := I;
            while I <= Expression'Last and then 
                  (Expression(I) in 'a'..'z' or 
                   Expression(I) in 'A'..'Z' or 
                   Expression(I) in '0'..'9') loop
               I := I + 1;
            end loop;
            
            declare
               Var_Name : Unbounded_String := 
                 To_Unbounded_String(Expression(Start..I-1));
            begin
               if not Variables.Contains(Var_Name) then
                  raise Undefined_Variable with 
                    "Undefined variable: " & Expression(Start..I-1);
               end if;
               
               return Variables.Element(Var_Name);
            end;
            
         -- Parse parenthesized expression
         elsif Expression(I) = '(' then
            I := I + 1;  -- Skip '('
            Result := Parse_Expression;
            
            Skip_Whitespace;
            if I > Expression'Last or else Expression(I) /= ')' then
               raise Syntax_Error with "Missing closing parenthesis";
            end if;
            
            I := I + 1;  -- Skip ')'
            return Result;
            
         else
            raise Syntax_Error with 
              "Unexpected character: " & Expression(I);
         end if;
      end Parse_Primary;
      
      -- Parse a factor (handle exponentiation)
      function Parse_Factor return Float is
         Left : Float := Parse_Primary;
      begin
         Skip_Whitespace;
         
         if I <= Expression'Last and then Expression(I) = '^' then
            I := I + 1;  -- Skip '^'
            declare
               Right : Float := Parse_Primary;
               Right_Int : Integer := Integer(Right);
            begin
               if Float(Right_Int) /= Right then
                  raise Syntax_Error with "Exponent must be an integer";
               end if;
               
               return Left ** Right_Int;
            end;
         else
            return Left;
         end if;
      end Parse_Factor;
      
      -- Parse a term (handle multiplication and division)
      function Parse_Term return Float is
         Left : Float := Parse_Factor;
      begin
         loop
            Skip_Whitespace;
            
            exit when I > Expression'Last;
            
            if Expression(I) = '*' then
               I := I + 1;  -- Skip '*'
               Left := Left * Parse_Factor;
            elsif Expression(I) = '/' then
               I := I + 1;  -- Skip '/'
               declare
                  Right : Float := Parse_Factor;
               begin
                  if Right = 0.0 then
                     raise Division_By_Zero;
                  end if;
                  
                  Left := Left / Right;
               end;
            else
               exit;
            end if;
         end loop;
         
         return Left;
      end Parse_Term;
      
      -- Parse an expression (handle addition and subtraction)
      function Parse_Expression return Float is
         Left : Float := Parse_Term;
      begin
         loop
            Skip_Whitespace;
            
            exit when I > Expression'Last;
            
            if Expression(I) = '+' then
               I := I + 1;  -- Skip '+'
               Left := Left + Parse_Term;
            elsif Expression(I) = '-' then
               I := I + 1;  -- Skip '-'
               Left := Left - Parse_Term;
            else
               exit;
            end if;
         end loop;
         
         return Left;
      end Parse_Expression;
      
   begin
      return Parse_Expression;
   end Evaluate;
   
   -- Parse and evaluate an assignment
   function Parse_Assignment (Input : String) return Float is
      Equal_Pos : Natural := 0;
   begin
      -- Find the equals sign
      for J in Input'Range loop
         if Input(J) = '=' then
            Equal_Pos := J;
            exit;
         end if;
      end loop;
      
      -- If no equals sign, it's not an assignment
      if Equal_Pos = 0 then
         return Evaluate(Input);
      end if;
      
      -- Extract variable name and expression
      declare
         Var_Name_Str : String := Input(Input'First..Equal_Pos-1);
         Expr_Str : String := Input(Equal_Pos+1..Input'Last);
         Var_Name : Unbounded_String;
         Value : Float;
         J : Positive := Var_Name_Str'First;
      begin
         -- Skip leading whitespace in variable name
         while J <= Var_Name_Str'Last and then Var_Name_Str(J) = ' ' loop
            J := J + 1;
         end loop;
         
         -- Extract variable name
         declare
            Start : Positive := J;
         begin
            while J <= Var_Name_Str'Last and then 
                  (Var_Name_Str(J) in 'a'..'z' or 
                   Var_Name_Str(J) in 'A'..'Z' or 
                   Var_Name_Str(J) in '0'..'9') loop
               J := J + 1;
            end loop;
            
            if Start > J-1 then
               raise Syntax_Error with "Invalid variable name";
            end if;
            
            Var_Name := To_Unbounded_String(Var_Name_Str(Start..J-1));
         end;
         
         -- Evaluate the expression
         Value := Evaluate(Expr_Str);
         
         -- Store the variable
         Variables.Include(Var_Name, Value);
         
         return Value;
      end;
   end Parse_Assignment;
   
begin
   Put_Line("Simple Ada Calculator");
   Put_Line("Enter expressions with variables and assignments (e.g., 'x = 5', 'y = x + 3')");
   Put_Line("Type 'exit' to quit");
   
   -- Main loop
   loop
      Put("> ");
      declare
         Input : String := Get_Line;
         Result : Float;
      begin
         exit when Input = "exit";
         
         if Input'Length > 0 then
            begin
               Result := Parse_Assignment(Input);
               Put(Result, Fore => 1, Aft => 2, Exp => 0);
               New_Line;
            exception
               when Syntax_Error =>
                  Put_Line("Error: Invalid syntax");
               when Undefined_Variable =>
                  Put_Line("Error: Undefined variable");
               when Division_By_Zero =>
                  Put_Line("Error: Division by zero");
               when Constraint_Error =>
                  Put_Line("Error: Invalid number format");
               when others =>
                  Put_Line("Error: Unknown error occurred");
            end;
         end if;
      end;
   end loop;
end Simple_Calculator;
