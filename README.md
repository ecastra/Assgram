
Assembly Language Grammar Documentation
Introduction

This document describes an enhanced assembly language designed for OS development and other performance-critical applications. It aims to improve upon traditional assembly languages by incorporating features that enhance readability, maintainability, and safety while retaining the low-level control and zero-overhead characteristics of assembly.

Key Features and Benefits

This assembly language offers several advantages over conventional assembly languages:

Zero-Overhead Abstraction: The language is designed to compile directly into efficient machine code with no hidden overhead from the compiler. All operations and memory accesses are explicit, giving the programmer complete control.

Modularity: Supports modules (::module_name:) and imports (:import :other_module:) for better code organization, reusability, and separate compilation.

Strong Typing: Features a rich type system, including basic types (u8, i8, u16, i16, u32, i32, u64, i64, f32, f64, ptr, void), user-defined struct and union types, pointers (type*), and arrays (type[size]).

Bitfields: Allows defining bitfields within structs, providing fine-grained control over memory layout and enabling easy manipulation of hardware registers.

Enhanced Control Flow: Includes if-else statements, while loops, do-while loops, and switch statements for more structured and readable code compared to traditional jump-based control flow.

Memory Safety Features:

Explicit memory access through the memory operand, making it easier to track and reason about memory operations.

Type specifiers in memory operands (e.g., [u32:%r0]) to ensure correct data access.

Assembler-enforced type checking to reduce type errors.

Macros: Supports parameterized macros with typed arguments, enabling code reuse and abstraction.

Conditional Assembly: Includes preprocessor directives (#if, #ifdef, #ifndef, #elif, #else, #endif) for conditional code compilation, making it easier to write portable and configurable assembly code.

Namespaces: Modules provide namespaces, preventing naming conflicts in larger projects.

Readability: Uses a more modern syntax (e.g., operand = expression for assignments) that is more intuitive and easier to read than traditional assembly mnemonics.

Language Overview
Program Structure

An assembly program consists of one or more modules. Each module can contain sections, declarations, and statements.

::module_a:

.code
  ; Code goes here

::

::module_b: :module_a:

.data
  ; Data goes here

::
content_copy
download
Use code with caution.
Assembly
Sections

.code: Contains executable code.

.data: Contains initialized data.

.rodata: Contains read-only data.

.bss: Contains uninitialized data.

Declarations

Labels: identifier:

Types: @identifier = type_specifier;

Constants: #identifier = expression;

Data: [&] identifier: type_specifier [(: bitfield_size) | ( "[" [positive_integer_literal] "]" )] [= initializer] [,];

Statements

Assignment: operand = expression;

Jump: je, jne, jl, jg, jle, jge, jmp

Call: [operand =] identifier([expression {, expression}]);

Return: ret [expression];

Privileged: ~privileged_instruction [operand {, operand}];

Loops: loop, while, do-while

If-Else: if comparison_expression statement [else statement]

Switch: switch (expression) { case immediate: statement [break] ... [default: statement] }

Floating-Point: floating_point_mnemonic [operand {, operand}];

Assembly Instruction: instruction_mnemonic [operand {, operand}];

Macro Call ! identifier ( typed_arg { , typed_arg } )

Comment: // ... or /* ... */

Operands

Registers: %r0 - %r15, %f0 - %f15, %sp, %bp, %err (architecture-specific)

Memory: [ [memory_segment:] (type_specifier | "&"): (register | (register [pointer_op expression]) | label | indexed_addressing) ]

Labels: identifier

Struct Members: operand.identifier[:bitfield_size]

Expressions

Operands

Immediate Values: Integer, character, string, or float literals.

Unary Operators: -, !, ~

Binary Operators: +, -, *, /, %, &, |, ^, <<, >>

Comparison Operators: ==, !=, <, >, <=, >=

Function Calls

Directives

.align: .align unsigned_integer_literal

.global: .global identifier

.extern: .extern identifier

.endian: .endian (little | big)

Conditional Assembly: #if, #ifdef, #ifndef, #elif, #else, #endif

.macro and .endm

EBNF Grammar
(* =================================================== *)
(*  Complete Assembly Language Grammar (Zero-Overhead, No _, Bitfields, Refinements)  *)
(* =================================================== *)

(* --- Program Structure --- *)
program = { module } ;

(* --- Module Definition --- *)
module = "::" identifier [ ":" { import } ] { section } "::" ;

(* --- Import Statement --- *)
import = ":" identifier ":" ;

(* --- Sections --- *)
section = code_section | data_section | rodata_section | bss_section;
code_section = ".code" { label_decl | statement | macro_def | type_decl | const_decl | directive };
data_section = ".data" { data_decl };
rodata_section = ".rodata" { data_decl };
bss_section = ".bss" { data_decl };

(* --- Declarations --- *)
label_decl = identifier ":" ;
type_decl = "@" identifier "=" type_specifier;
const_decl = "#" identifier "=" expression;

(* --- Macro Definition --- *)
macro_def = "!" identifier "(" [ typed_arg { "," typed_arg } ] ")" "{" { statement } "}" ;
typed_arg = identifier ":" type_specifier;

(* --- Data Declaration --- *)
data_decl = [ "&" ] identifier ":" type_specifier [ (":" bitfield_size) | ( "[" [positive_integer_literal] "]" ) ] [ "=" initializer ] [ ("," | ";") ] ;

(* --- Type Specifiers --- *)
type_specifier = basic_type | struct_type | union_type | pointer_type | array_type | identifier ;
basic_type = "u8" | "i8" | "u16" | "i16" | "u32" | "i32" | "u64" | "i64" | "ptr" | "void" | "f32" | "f64";
struct_type = "struct" [ "{" { field_declaration } "}" ];
field_declaration = identifier ":" type_specifier [ ":" bitfield_size ] ";" ;
bitfield_size = positive_integer_literal;
union_type = "union" [ "{" { identifier ":" type_specifier ";" } "}" ];
pointer_type = type_specifier "*" ;
array_type = type_specifier "[" positive_integer_literal "]" ;

(* --- Initializer --- *)
initializer = expression | "{" [field_initializer { "," field_initializer }] "}" ;
field_initializer = [ identifier ":" ] expression;

(* --- Statements --- *)
statement = assignment_statement | jump_statement | call_statement | return_statement | privileged_statement | loop_statement | ifelse_statement | switch_statement | comment | floating_point_statement | macro_call | assembly_instruction | ";" ;

(* --- Assignment Statement --- *)
assignment_statement = operand "=" expression ";" ;

(* --- Conditional Statement --- *)
ifelse_statement =  "if" comparison_expression [ statement ] [ "else" statement ]

(* --- Loop Statement --- *)
loop_statement = "loop" "(" [assignment_statement] comparison_expression ";" [assignment_statement] ")" statement
               | "while" "(" comparison_expression ")" statement
               | "do" statement "while" "(" comparison_expression ")" ";" ;

(* --- Jump Statement --- *)
jump_statement = ("je" | "jne" | "jl" | "jg" | "jle" | "jge" | "jmp") label ";" ;

(* --- Switch Statement --- *)
switch_statement = "switch" "(" expression ")" "{" { case_label statement } [ "default" ":" statement ] "}" ;
case_label = "case" immediate ":" ;

(* --- Function Call --- *)
call_statement = [ operand "=" ] identifier "(" [ expression { "," expression } ] ")" ";" ;

(* --- Return Statement --- *)
return_statement = "ret" [ expression ] ";" ;

(* --- Privileged Statement --- *)
privileged_statement = "~" ( privileged_instruction | "in" | "out" | "sti" | "cli" | "lidt" | "lgdt" | "invlpg") [ operand { "," operand } ] ";" ;

(* --- Floating-Point Statement --- *)
floating_point_statement = floating_point_mnemonic [ operand { "," operand } ] ";" ;

(* --- Assembly Instruction --- *)
assembly_instruction = instruction_mnemonic [ operand { "," operand } ] ";" ;

(* --- Operands --- *)
operand = register | memory | label | struct_member;
struct_member = operand "." identifier [ ":" bitfield_size ];

(* --- Memory Addressing --- *)
memory = "[" [ memory_segment ":"] ( type_specifier | "&" ) ":" ( register | ( register [ pointer_op expression ] ) | label | indexed_addressing ) "]" ;
indexed_addressing = register [ "+" register [ "*" ( "1" | "2" | "4" | "8" ) ] ] [ "+" expression ]

(* --- Comparison Expression --- *)
comparison_expression = operand comparison_operator operand;

(* --- Expressions --- *)
expression = operand
           | immediate
           | unary_op expression
           | expression binary_op expression
           | "(" expression ")"
           | call_statement;

(* --- Unary Operators --- *)
unary_op = "-" | "!" | "~";

(* --- Binary Operators --- *)
binary_op = "+" | "-" | "*" | "/" | "%" | "&" | "|" | "^" | "<<" | ">>" ;

(* --- Comparison Operators --- *)
comparison_operator = "==" | "!=" | "<" | ">" | "<=" | ">=";

(* --- Registers (Architecture-Specific) --- *)
register = general_purpose_register | floating_point_register | special_register ;
general_purpose_register =  "%" ( "r" ( "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" |
                      "8" | "9" | "10" | "11" | "12" | "13" | "14" | "15" )) ;
floating_point_register = "%" ( "f" ( "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" |
                                    "8" | "9" | "10" | "11" | "12" | "13" | "14" | "15" )) ;
special_register = "%sp" | "%bp" | "%err" ;

(* --- Memory Segment --- *)
memory_segment = (* ... Architecture-specific segment registers or segment selectors ... *)
                 "%cs" | "%ds" | "%ss" | "%es" | "%fs" | "%gs";

(* --- Pointer Operations --- *)
pointer_op = "+" | "-";

(* --- Immediate Values --- *)
immediate = integer_literal | character_literal | string_literal | float_literal;

(* --- Literals --- *)
integer_literal = ["+"|"-"] digit {digit} | "0x" hex_digit {hex_digit} | "0b" binary_digit {binary_digit};
positive_integer_literal = digit {digit} | "0x" hex_digit {hex_digit} | "0b" binary_digit {binary_digit};
unsigned_integer_literal = digit {digit} | "0x" hex_digit {hex_digit} | "0b" binary_digit {binary_digit};
hex_digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "a" | "b" | "c" | "d" | "e" | "f";
binary_digit = "0" | "1";
character_literal = "'" ( any_character_except_quote | escape_sequence ) "'";
string_literal = '"' { any_character_except_quote | escape_sequence } '"';
escape_sequence = "\\" ( "'" | '"' | "\\" | "n" | "r" | "t" | "b" | "f" | "v" | "0" | "x" hex_digit hex_digit );
float_literal = ["+"|"-"] digit {digit} "." digit {digit} [("e"|"E") ["+"|"-"] digit {digit}];

(* --- Comments --- *)
comment = "//" { any_character } newline | "/*" { any_character } "*/" ;

(* --- Floating-Point Mnemonics (Architecture-Specific) --- *)
floating_point_mnemonic = (* ... Architecture-specific floating-point instructions ... *)
                          "fadd" | "fsub" | "fmul" | "fdiv" | "fsqrt" | "fsin" | "fcos" | "ftan";

(* --- Privileged Instructions (Architecture-Specific) --- *)
privileged_instruction = (* ... Architecture-specific privileged instructions ... *)
                          "lidt" | "lgdt" | "invlpg";

(* --- Directives --- *)
directive = conditional_assembly_directive | data_directive | macro_directive;

data_directive = "." ( "align" unsigned_integer_literal | "global" identifier | "extern" identifier | "endian" ( "little" | "big" ) ) ;

conditional_assembly_directive = "#" ("if" | "ifdef" | "ifndef" | "elif" | "else" | "endif") [ expression ];

macro_directive = "." ( "macro" | "endm" );

(* --- Basic Elements --- *)
digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
identifier = letter { letter | digit };
letter = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" |
         "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z";
any_character_except_quote = [^"'"] ;
any_character_except_doublequote = [^'"'];
any_character = /* ASCII 0x00 - 0x7F */;
newline = /* 0x0A | 0x0D 0x0A */;
content_copy
download
Use code with caution.
Ebnf
Example Program
::my_module: :io:

.code

// Macro definition
.macro my_macro(%arg1: u32, %arg2: u32)
  mov %r0, %arg1
  add %r0, %arg2
.endm

@my_struct = struct {
  x: u32,
  y: i16,
  flags: u8:4
};

#if defined(DEBUG)
  #LOG_LEVEL = 1  // Debug logging
#elif defined(RELEASE)
  #LOG_LEVEL = 2  // Release logging
#else
  #LOG_LEVEL = 0  // No logging
#endif

#MAX_COUNT = 10

main:
  %r0 = 5
  %r1 = 10
  %r2 = %r0 + %r1

  &my_data: my_struct = { x: 123, y: -42, flags: 0b1100 }

#if #LOG_LEVEL > 0 
  // Log something if logging is enabled
  print_string("Entering main function")
#endif

  if %r2 > #MAX_COUNT
    %r3 = 1
  else
    %r3 = 0

  // Macro call
  my_macro(%r0, %r1) // Expands to: mov %r0, %r0; add %r0, %r1

  // Using a switch statement
  switch %r1
  case 1:
    %r5 = 10
    break
  case 2:
    %r5 = 20
    break
  default:
    %r5 = 0
  
  while %r0 < %r1 {
    %r0 = %r0 + 1
  }

  ret

.data
.align 4 // Align the next data to a 4-byte boundary
&my_data: my_struct

.rodata
&message: ptr = "Hello, world!"

.bss
&buffer: u8[128]

::
content_copy
download
Use code with caution.
Assembly
Conclusion

This documentation provides a comprehensive overview of the enhanced assembly language grammar. By combining the readability and safety features with the low-level control of assembly, this language aims to be a powerful tool for OS development and other performance-sensitive applications. The EBNF grammar provides a precise specification for building an assembler, and the example program demonstrates the language's capabilities. Remember to fill in the architecture-specific details and implement the necessary assembler components (instruction encoding, macro expansion, type checking, error handling) to bring this language to life.
