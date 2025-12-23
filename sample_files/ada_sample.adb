-- ==============================================================================
-- Comprehensive Ada Sample - Syntax Highlighting Demonstration
-- ==============================================================================

-- This file demonstrates all major Ada language features
-- for syntax highlighting purposes.

-- ==============================================================================
-- Comments
-- ==============================================================================

-- Single line comment

-- Multiple
-- line
-- comments

-- ==============================================================================
-- Context Clauses (With/Use)
-- ==============================================================================

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Numerics.Float_Random;
with Ada.Calendar;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Ada.Finalization;
with Ada.Streams;
with System;

use Ada.Text_IO;
use Ada.Strings.Unbounded;

-- ==============================================================================
-- Package Specification
-- ==============================================================================

package Sample_Package is

   -- Constants
   Max_Size : constant := 100;
   Pi : constant := 3.14159_26535_89793;
   Version : constant String := "1.0.0";

   -- Enumeration types
   type Day_Of_Week is (Monday, Tuesday, Wednesday, Thursday,
                        Friday, Saturday, Sunday);

   type Color is (Red, Green, Blue, Yellow, Cyan, Magenta);
   for Color use (Red => 1, Green => 2, Blue => 4,
                  Yellow => 8, Cyan => 16, Magenta => 32);

   -- Subtypes
   subtype Weekday is Day_Of_Week range Monday .. Friday;
   subtype Weekend is Day_Of_Week range Saturday .. Sunday;
   subtype Positive_Float is Float range 0.0 .. Float'Last;
   subtype Percentage is Integer range 0 .. 100;

   -- Numeric types
   type Byte is mod 256;
   type Word is mod 2**16;
   type Long_Word is mod 2**32;

   type Fixed_Point is delta 0.01 range -1000.0 .. 1000.0;
   type Decimal_Type is delta 0.01 digits 10;

   -- Array types
   type Int_Array is array (Positive range <>) of Integer;
   type Fixed_Array is array (1 .. 10) of Float;
   type Matrix is array (1 .. 3, 1 .. 3) of Float;
   type String_10 is array (1 .. 10) of Character;

   -- Record types
   type Person is record
      Name : Unbounded_String;
      Age  : Natural;
      Active : Boolean := True;
   end record;

   -- Discriminated record
   type Buffer (Size : Positive) is record
      Data : String (1 .. Size);
      Length : Natural := 0;
   end record;

   -- Variant record
   type Shape_Kind is (Circle, Rectangle, Triangle);
   type Shape (Kind : Shape_Kind) is record
      X, Y : Float;
      case Kind is
         when Circle =>
            Radius : Positive_Float;
         when Rectangle =>
            Width, Height : Positive_Float;
         when Triangle =>
            Base, Altitude : Positive_Float;
      end case;
   end record;

   -- Access types (pointers)
   type Int_Ptr is access Integer;
   type Person_Ptr is access Person;
   type Int_Array_Ptr is access Int_Array;
   type String_Ptr is access String;
   type Const_String_Ptr is access constant String;

   -- Private types (declared here, defined in body)
   type Stack is private;
   type Queue is limited private;

   -- Tagged types (OOP)
   type Animal is tagged record
      Name : Unbounded_String;
      Age  : Natural;
   end record;

   type Dog is new Animal with record
      Breed : Unbounded_String;
   end record;

   type Cat is new Animal with record
      Indoor : Boolean;
   end record;

   -- Abstract types
   type Drawable is abstract tagged null record;

   -- Interface types
   type Printable is interface;
   type Serializable is interface;
   type Combined is interface and Printable and Serializable;

   -- Generic instantiation
   package Int_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Integer);

   -- Subprogram declarations
   procedure Say_Hello;
   procedure Print_Person (P : in Person);
   function Add (A, B : Integer) return Integer;
   function Factorial (N : Natural) return Positive;

   -- Overloaded operators
   function "+" (Left, Right : Person) return Person;
   function "=" (Left, Right : Person) return Boolean;

   -- Generic procedure
   generic
      type Element_Type is private;
      with function "<" (L, R : Element_Type) return Boolean is <>;
   procedure Generic_Sort (Arr : in out Int_Array);

   -- Exceptions
   Invalid_Input : exception;
   Buffer_Overflow : exception;
   Not_Found : exception;

private

   type Stack is record
      Data : Int_Array (1 .. Max_Size);
      Top  : Natural := 0;
   end record;

   type Queue is record
      Data  : Int_Array (1 .. Max_Size);
      Front : Positive := 1;
      Rear  : Natural := 0;
      Count : Natural := 0;
   end record;

end Sample_Package;

-- ==============================================================================
-- Package Body
-- ==============================================================================

package body Sample_Package is

   -- Local variables
   Counter : Integer := 0;
   Initialized : Boolean := False;

   -- Procedure implementation
   procedure Say_Hello is
   begin
      Put_Line ("Hello, Ada!");
   end Say_Hello;

   procedure Print_Person (P : in Person) is
   begin
      Put ("Name: ");
      Put_Line (To_String (P.Name));
      Put ("Age: ");
      Ada.Integer_Text_IO.Put (P.Age, Width => 0);
      New_Line;
   end Print_Person;

   -- Function implementation
   function Add (A, B : Integer) return Integer is
   begin
      return A + B;
   end Add;

   -- Recursive function
   function Factorial (N : Natural) return Positive is
   begin
      if N <= 1 then
         return 1;
      else
         return N * Factorial (N - 1);
      end if;
   end Factorial;

   -- Operator overloading
   function "+" (Left, Right : Person) return Person is
   begin
      return (Name   => Left.Name & To_Unbounded_String (" & ") & Right.Name,
              Age    => (Left.Age + Right.Age) / 2,
              Active => Left.Active and Right.Active);
   end "+";

   function "=" (Left, Right : Person) return Boolean is
   begin
      return Left.Name = Right.Name and then Left.Age = Right.Age;
   end "=";

   -- Generic procedure body
   procedure Generic_Sort (Arr : in out Int_Array) is
      Temp : Integer;
   begin
      for I in Arr'First .. Arr'Last - 1 loop
         for J in I + 1 .. Arr'Last loop
            if Arr (J) < Arr (I) then
               Temp := Arr (I);
               Arr (I) := Arr (J);
               Arr (J) := Temp;
            end if;
         end loop;
      end loop;
   end Generic_Sort;

end Sample_Package;

-- ==============================================================================
-- Main Procedure
-- ==============================================================================

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Calendar;
with Ada.Numerics.Float_Random;
with Sample_Package;

procedure Sample_Main is

   -- ===========================================================================
   -- Variable Declarations
   -- ===========================================================================

   -- Basic types
   I, J, K : Integer;
   X, Y, Z : Float;
   B : Boolean := True;
   C : Character := 'A';
   S : String (1 .. 20) := (others => ' ');
   U : Unbounded_String := To_Unbounded_String ("Hello");

   -- Numeric literals
   Decimal_Num : Integer := 42;
   Based_Num : Integer := 16#FF#;
   Binary_Num : Integer := 2#1010_1010#;
   Octal_Num : Integer := 8#777#;
   Float_Num : Float := 3.14159;
   Exp_Num : Float := 6.022E23;
   Neg_Exp : Float := 1.0E-10;

   -- Using custom types
   Day : Sample_Package.Day_Of_Week := Sample_Package.Monday;
   Person1 : Sample_Package.Person;

   -- Array initialization
   Numbers : Sample_Package.Fixed_Array := (1.0, 2.0, 3.0, others => 0.0);
   Matrix1 : Sample_Package.Matrix := (others => (others => 0.0));

   -- Access types
   Ptr : Sample_Package.Int_Ptr;
   Str_Ptr : Sample_Package.String_Ptr;

   -- Controlled types for finalization
   type Controlled_Wrapper is new Ada.Finalization.Controlled with record
      Value : Integer;
   end record;

   overriding procedure Initialize (Object : in out Controlled_Wrapper);
   overriding procedure Finalize (Object : in Out Controlled_Wrapper);

   procedure Initialize (Object : in Out Controlled_Wrapper) is
   begin
      Object.Value := 0;
      Put_Line ("Initialized");
   end Initialize;

   procedure Finalize (Object : in Out Controlled_Wrapper) is
   begin
      Put_Line ("Finalized");
   end Finalize;

   -- ===========================================================================
   -- Local Procedures and Functions
   -- ===========================================================================

   -- Procedure with different parameter modes
   procedure Swap (A, B : in Out Integer) is
      Temp : Integer;
   begin
      Temp := A;
      A := B;
      B := Temp;
   end Swap;

   -- Function with expression function (Ada 2012)
   function Square (N : Integer) return Integer is (N * N);

   function Is_Even (N : Integer) return Boolean is (N mod 2 = 0);

   -- Procedure with default parameters
   procedure Greet (Name : String := "World";
                    Times : Positive := 1) is
   begin
      for I in 1 .. Times loop
         Put_Line ("Hello, " & Name & "!");
      end loop;
   end Greet;

   -- Nested procedure
   procedure Outer is
      Local_Var : Integer := 10;

      procedure Inner is
      begin
         Local_Var := Local_Var + 1;  -- Access outer scope
         Put_Line ("Inner: " & Integer'Image (Local_Var));
      end Inner;

   begin
      Inner;
      Put_Line ("Outer: " & Integer'Image (Local_Var));
   end Outer;

   -- ===========================================================================
   -- Begin Main
   -- ===========================================================================

begin
   Put_Line ("=== Ada Sample Program ===");
   New_Line;

   -- ===========================================================================
   -- Control Flow
   -- ===========================================================================

   -- If statement
   I := 42;
   if I > 0 then
      Put_Line ("Positive");
   elsif I < 0 then
      Put_Line ("Negative");
   else
      Put_Line ("Zero");
   end if;

   -- Case statement
   case Day is
      when Sample_Package.Monday =>
         Put_Line ("Start of week");
      when Sample_Package.Friday =>
         Put_Line ("TGIF!");
      when Sample_Package.Saturday | Sample_Package.Sunday =>
         Put_Line ("Weekend!");
      when others =>
         Put_Line ("Midweek");
   end case;

   -- ===========================================================================
   -- Loops
   -- ===========================================================================

   -- Simple loop
   I := 0;
   loop
      I := I + 1;
      exit when I >= 5;
   end loop;

   -- While loop
   I := 0;
   while I < 5 loop
      Put (Integer'Image (I) & " ");
      I := I + 1;
   end loop;
   New_Line;

   -- For loop (forward)
   for Idx in 1 .. 5 loop
      Put (Integer'Image (Idx) & " ");
   end loop;
   New_Line;

   -- For loop (reverse)
   for Idx in reverse 1 .. 5 loop
      Put (Integer'Image (Idx) & " ");
   end loop;
   New_Line;

   -- For loop with range
   for Day_Idx in Sample_Package.Day_Of_Week loop
      Put_Line (Sample_Package.Day_Of_Week'Image (Day_Idx));
   end loop;

   -- For loop over array
   for Elem of Numbers loop
      Ada.Float_Text_IO.Put (Elem, Fore => 1, Aft => 2, Exp => 0);
      Put (" ");
   end loop;
   New_Line;

   -- Named loop with exit
   Outer_Loop :
   for I in 1 .. 10 loop
      Inner_Loop :
      for J in 1 .. 10 loop
         if I * J > 50 then
            exit Outer_Loop;
         end if;
         Put (Integer'Image (I * J) & " ");
      end loop Inner_Loop;
      New_Line;
   end loop Outer_Loop;

   -- ===========================================================================
   -- Exception Handling
   -- ===========================================================================

   declare
      Result : Integer;
   begin
      Result := 10 / 0;  -- Will raise Constraint_Error
   exception
      when Constraint_Error =>
         Put_Line ("Caught Constraint_Error!");
      when Sample_Package.Invalid_Input =>
         Put_Line ("Caught Invalid_Input!");
      when E : others =>
         Put_Line ("Caught exception: " &
                   Ada.Exceptions.Exception_Name (E));
         Put_Line (Ada.Exceptions.Exception_Message (E));
   end;

   -- Raise exception
   begin
      raise Sample_Package.Invalid_Input with "Test exception";
   exception
      when E : Sample_Package.Invalid_Input =>
         Put_Line ("Message: " & Ada.Exceptions.Exception_Message (E));
   end;

   -- ===========================================================================
   -- Dynamic Memory
   -- ===========================================================================

   -- Allocate
   Ptr := new Integer'(42);
   Put_Line ("Ptr value: " & Integer'Image (Ptr.all));

   Str_Ptr := new String'("Dynamic string");
   Put_Line (Str_Ptr.all);

   -- Deallocate
   declare
      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Integer, Name => Sample_Package.Int_Ptr);
      procedure Free_String is new Ada.Unchecked_Deallocation
        (Object => String, Name => Sample_Package.String_Ptr);
   begin
      Free (Ptr);
      Free_String (Str_Ptr);
   end;

   -- ===========================================================================
   -- Attributes
   -- ===========================================================================

   Put_Line ("Integer'First: " & Integer'Image (Integer'First));
   Put_Line ("Integer'Last: " & Integer'Image (Integer'Last));
   Put_Line ("Integer'Size: " & Integer'Image (Integer'Size));

   Put_Line ("Float'Digits: " & Integer'Image (Float'Digits));
   Put_Line ("Float'Epsilon: " & Float'Image (Float'Epsilon));

   Put_Line ("Array'First: " & Integer'Image (Numbers'First));
   Put_Line ("Array'Last: " & Integer'Image (Numbers'Last));
   Put_Line ("Array'Length: " & Integer'Image (Numbers'Length));
   Put_Line ("Array'Range: " & Integer'Image (Numbers'First) & ".." &
             Integer'Image (Numbers'Last));

   -- Type conversions
   I := Integer (X);
   X := Float (I);
   S := String'("Hello Ada!        ");

   -- ===========================================================================
   -- Block Statement
   -- ===========================================================================

   declare
      Local_Var : Integer := 100;
      type Local_Type is (A, B, C);
   begin
      Local_Var := Local_Var + 1;
      Put_Line ("Local: " & Integer'Image (Local_Var));
   end;

   -- ===========================================================================
   -- Calendar Operations
   -- ===========================================================================

   declare
      use Ada.Calendar;
      Now : Time := Clock;
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Seconds : Day_Duration;
   begin
      Split (Now, Year, Month, Day, Seconds);
      Put_Line ("Date: " & Year_Number'Image (Year) & "-" &
                Month_Number'Image (Month) & "-" &
                Day_Number'Image (Day));
   end;

   -- ===========================================================================
   -- Random Numbers
   -- ===========================================================================

   declare
      Gen : Ada.Numerics.Float_Random.Generator;
      R : Float;
   begin
      Ada.Numerics.Float_Random.Reset (Gen);
      R := Ada.Numerics.Float_Random.Random (Gen);
      Put ("Random: ");
      Ada.Float_Text_IO.Put (R, Fore => 1, Aft => 4, Exp => 0);
      New_Line;
   end;

   -- ===========================================================================
   -- Aggregate Expressions
   -- ===========================================================================

   Person1 := (Name   => To_Unbounded_String ("John"),
               Age    => 30,
               Active => True);

   Numbers := (1 | 3 | 5 => 1.0, 2 | 4 => 2.0, others => 0.0);

   Matrix1 := (1 => (1 => 1.0, others => 0.0),
               2 => (2 => 1.0, others => 0.0),
               3 => (3 => 1.0, others => 0.0));

   -- ===========================================================================
   -- Pragma Directives
   -- ===========================================================================

   pragma Inline (Square);
   pragma Optimize (Time);
   pragma Warnings (Off);
   pragma Assert (I > 0, "I must be positive");

   Put_Line ("=== Program Complete ===");

end Sample_Main;

-- ==============================================================================
-- Task and Protected Types (Concurrency)
-- ==============================================================================

package Concurrency_Sample is

   -- Task type declaration
   task type Worker is
      entry Start (Id : Integer);
      entry Stop;
   end Worker;

   -- Protected type declaration
   protected type Semaphore is
      entry Acquire;
      procedure Release;
      function Is_Available return Boolean;
   private
      Available : Boolean := True;
   end Semaphore;

   -- Protected object
   protected Shared_Counter is
      procedure Increment;
      procedure Decrement;
      function Value return Integer;
   private
      Count : Integer := 0;
   end Shared_Counter;

end Concurrency_Sample;

package body Concurrency_Sample is

   task body Worker is
      My_Id : Integer;
      Running : Boolean := False;
   begin
      accept Start (Id : Integer) do
         My_Id := Id;
         Running := True;
      end Start;

      while Running loop
         select
            accept Stop do
               Running := False;
            end Stop;
         or
            delay 1.0;
            Put_Line ("Worker" & Integer'Image (My_Id) & " working...");
         end select;
      end loop;
   end Worker;

   protected body Semaphore is
      entry Acquire when Available is
      begin
         Available := False;
      end Acquire;

      procedure Release is
      begin
         Available := True;
      end Release;

      function Is_Available return Boolean is
      begin
         return Available;
      end Is_Available;
   end Semaphore;

   protected body Shared_Counter is
      procedure Increment is
      begin
         Count := Count + 1;
      end Increment;

      procedure Decrement is
      begin
         Count := Count - 1;
      end Decrement;

      function Value return Integer is
      begin
         return Count;
      end Value;
   end Shared_Counter;

end Concurrency_Sample;

-- ==============================================================================
-- Generic Package Example
-- ==============================================================================

generic
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Generic_Stack is

   type Stack (Max_Size : Positive) is private;

   procedure Push (S : in Out Stack; Item : Element_Type);
   procedure Pop (S : in Out Stack; Item : Out Element_Type);
   function Top (S : Stack) return Element_Type;
   function Is_Empty (S : Stack) return Boolean;
   function Is_Full (S : Stack) return Boolean;

   Stack_Overflow : exception;
   Stack_Underflow : exception;

private

   type Element_Array is array (Positive range <>) of Element_Type;

   type Stack (Max_Size : Positive) is record
      Data : Element_Array (1 .. Max_Size);
      Top_Index : Natural := 0;
   end record;

end Generic_Stack;

package body Generic_Stack is

   procedure Push (S : in Out Stack; Item : Element_Type) is
   begin
      if Is_Full (S) then
         raise Stack_Overflow;
      end if;
      S.Top_Index := S.Top_Index + 1;
      S.Data (S.Top_Index) := Item;
   end Push;

   procedure Pop (S : in Out Stack; Item : Out Element_Type) is
   begin
      if Is_Empty (S) then
         raise Stack_Underflow;
      end if;
      Item := S.Data (S.Top_Index);
      S.Top_Index := S.Top_Index - 1;
   end Pop;

   function Top (S : Stack) return Element_Type is
   begin
      if Is_Empty (S) then
         raise Stack_Underflow;
      end if;
      return S.Data (S.Top_Index);
   end Top;

   function Is_Empty (S : Stack) return Boolean is
   begin
      return S.Top_Index = 0;
   end Is_Empty;

   function Is_Full (S : Stack) return Boolean is
   begin
      return S.Top_Index = S.Max_Size;
   end Is_Full;

end Generic_Stack;
