#!/usr/bin/perl
use strict;
use warnings;

# Simple Calculator Interpreter
# Supports variables, assignments, and basic arithmetic operations

my %variables = (); # Hash to store variables

# Main loop - command line interface
print "Simple Calculator\n";
print "Type 'exit' or 'quit' to exit\n";
print "> ";

while (my $input = <STDIN>) {
    chomp($input);
    
    # Check for exit command
    last if $input =~ /^(exit|quit)$/i;
    
    # Skip empty inputs
    if ($input =~ /^\s*$/) {
        print "> ";
        next;
    }
    
    # Interpret input and print result
    my $result = evaluate($input);
    if (defined $result) {
        print "= $result\n";
    }
    
    print "> ";
}

print "Calculator closing...\n";

# Parse and evaluate input
sub evaluate {
    my ($expression) = @_;
    
    # Check for assignment operation (e.g., x = 5)
    if ($expression =~ /^\s*([a-zA-Z][a-zA-Z0-9]*)\s*=\s*(.+)$/) {
        my $var_name = $1;
        my $var_expr = $2;
        
        # Calculate the value to be assigned
        my $value = parse_expression($var_expr);
        
        # Check for error condition
        return undef unless defined $value;
        
        # Assign value to variable
        $variables{$var_name} = $value;
        return $value;
    }
    
    # Normal expression evaluation
    return parse_expression($expression);
}

# Parse expression
sub parse_expression {
    my ($expr) = @_;
    
    # Parse expression for addition and subtraction
    my @terms = split_with_parentheses($expr, qr/[\+\-]/);
    
    # Evaluate first term
    my $result = parse_term(shift @terms);
    return undef unless defined $result;
    
    # Apply addition and subtraction operations
    while (@terms) {
        my $operator = shift @terms;
        my $term_value = parse_term(shift @terms);
        
        # Check for error condition
        return undef unless defined $term_value;
        
        if ($operator eq '+') {
            $result += $term_value;
        } elsif ($operator eq '-') {
            $result -= $term_value;
        }
    }
    
    return $result;
}

# Split expression with respect to operators, preserving parenthesized expressions
sub split_with_parentheses {
    my ($expr, $operator_regex) = @_;
    
    my @result = ();
    my $current = '';
    my $paren_level = 0;
    
    for my $char (split //, $expr) {
        if ($char eq '(') {
            $paren_level++;
            $current .= $char;
        }
        elsif ($char eq ')') {
            $paren_level--;
            if ($paren_level < 0) {
                print "Error: Unbalanced parentheses\n";
                return ();
            }
            $current .= $char;
        }
        elsif ($paren_level == 0 && $char =~ $operator_regex) {
            push @result, $current, $char;
            $current = '';
        }
        else {
            $current .= $char;
        }
    }
    
    if ($paren_level != 0) {
        print "Error: Unbalanced parentheses\n";
        return ();
    }
    
    push @result, $current if $current ne '';
    return @result;
}

# Parse term for multiplication and division
sub parse_term {
    my ($term) = @_;
    
    # Parse term for multiplication and division
    my @factors = split_with_parentheses($term, qr/[\*\/]/);
    
    # Evaluate first factor
    my $result = parse_factor(shift @factors);
    return undef unless defined $result;
    
    # Apply multiplication and division operations
    while (@factors) {
        my $operator = shift @factors;
        my $factor_value = parse_factor(shift @factors);
        
        # Check for error condition
        return undef unless defined $factor_value;
        
        if ($operator eq '*') {
            $result *= $factor_value;
        } elsif ($operator eq '/') {
            if ($factor_value == 0) {
                print "Error: Division by zero\n";
                return undef;
            }
            $result /= $factor_value;
        }
    }
    
    return $result;
}

# Parse basic factors (numbers, variables, parenthesized expressions)
sub parse_factor {
    my ($factor) = @_;
    $factor =~ s/^\s+|\s+$//g;  # Remove leading and trailing whitespace
    
    # Parenthesized expression
    if ($factor =~ /^\s*\((.*)\)\s*$/) {
        return parse_expression($1);
    }
    
    # Number
    if ($factor =~ /^-?\d+(\.\d+)?$/) {
        return $factor;
    }
    
    # Variable
    if ($factor =~ /^([a-zA-Z][a-zA-Z0-9]*)$/) {
        my $var_name = $1;
        if (exists $variables{$var_name}) {
            return $variables{$var_name};
        } else {
            print "Error: Undefined variable '$var_name'\n";
            return undef;
        }
    }
    
    print "Error: Invalid expression '$factor'\n";
    return undef;
}