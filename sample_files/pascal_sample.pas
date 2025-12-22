{==============================================================================}
{ Comprehensive Pascal Sample - Syntax Highlighting Demonstration             }
{==============================================================================}

{ This file demonstrates all major Pascal (Free Pascal/Delphi) language       }
{ features for syntax highlighting purposes.                                  }

{==============================================================================}
{ Comments                                                                     }
{==============================================================================}

// Single line comment (Delphi/FPC style)

{ Block comment
  spanning multiple lines }

(* Alternative block comment
   also spanning multiple lines *)

{$mode objfpc}        { Compiler directive }
{$H+}                 { Long strings }
{$IFDEF DEBUG}
  {$DEFINE LOGGING}
{$ENDIF}

{==============================================================================}
{ Program Header                                                               }
{==============================================================================}

program SampleProgram;

uses
  SysUtils, Classes, Math, TypInfo, Variants, DateUtils;

{==============================================================================}
{ Constants                                                                    }
{==============================================================================}

const
  { Numeric constants }
  MaxValue = 100;
  Pi = 3.14159265358979;
  E = 2.71828182845905;
  GoldenRatio = 1.61803398875;

  { Typed constants (initialized variables) }
  Counter: Integer = 0;
  Initialized: Boolean = False;

  { String constants }
  AppName = 'Sample Application';
  Version = '1.0.0';
  NewLine = #13#10;

  { Hexadecimal and binary }
  HexValue = $DEADBEEF;
  BinaryValue = %10101010;
  OctalValue = &777;

  { Set constant }
  Vowels = ['A', 'E', 'I', 'O', 'U', 'a', 'e', 'i', 'o', 'u'];

  { Array constant }
  DaysInMonth: array[1..12] of Integer = (
    31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
  );

{==============================================================================}
{ Type Definitions                                                             }
{==============================================================================}

type
  { Enumeration type }
  TDayOfWeek = (dwSunday, dwMonday, dwTuesday, dwWednesday,
                dwThursday, dwFriday, dwSaturday);

  TColor = (clRed, clGreen, clBlue, clYellow, clCyan, clMagenta);

  TDirection = (North, East, South, West);

  { Subrange types }
  TMonth = 1..12;
  TPercentage = 0..100;
  TUpperCase = 'A'..'Z';
  TWeekday = dwMonday..dwFriday;

  { Set type }
  TCharSet = set of Char;
  TDaySet = set of TDayOfWeek;
  TByteSet = set of Byte;

  { Array types }
  TIntArray = array of Integer;
  TStringArray = array of string;
  TFixedArray = array[1..10] of Double;
  TMatrix = array[1..3, 1..3] of Double;
  TMultiDim = array[0..9, 0..9, 0..9] of Integer;

  { Pointer types }
  PInteger = ^Integer;
  PDouble = ^Double;
  PString = ^string;
  PChar = ^Char;

  { Record types }
  TPoint = record
    X, Y: Integer;
  end;

  TRectangle = record
    Left, Top, Right, Bottom: Integer;
  end;

  TPerson = record
    FirstName: string[50];
    LastName: string[50];
    Age: Integer;
    BirthDate: TDateTime;
    Active: Boolean;
  end;

  { Record with methods (advanced record) }
  TVector = record
    X, Y, Z: Double;
    class function Create(AX, AY, AZ: Double): TVector; static;
    function Length: Double;
    function Normalize: TVector;
    class operator +(const A, B: TVector): TVector;
    class operator *(const V: TVector; S: Double): TVector;
  end;

  { Variant record }
  TShapeKind = (skCircle, skRectangle, skTriangle);

  TShape = record
    PosX, PosY: Double;
    case Kind: TShapeKind of
      skCircle: (Radius: Double);
      skRectangle: (Width, Height: Double);
      skTriangle: (Base, Altitude: Double);
  end;

  { Procedural types }
  TCompareFunc = function(A, B: Integer): Integer;
  TNotifyProc = procedure(Sender: TObject);
  TStringFunc = function(const S: string): string;

  { Method pointers }
  TMethodProc = procedure of object;
  TEventHandler = procedure(Sender: TObject) of object;

  { Class forward declaration }
  TNode = class;

  { Pointer to class }
  PNode = ^TNode;

  { Class types }
  TAnimal = class
  private
    FName: string;
    FAge: Integer;
  protected
    procedure SetName(const Value: string); virtual;
    function GetDescription: string; virtual;
  public
    constructor Create(const AName: string; AAge: Integer);
    destructor Destroy; override;
    procedure Speak; virtual; abstract;
    property Name: string read FName write SetName;
    property Age: Integer read FAge write FAge;
    property Description: string read GetDescription;
  end;

  TDog = class(TAnimal)
  private
    FBreed: string;
  public
    constructor Create(const AName: string; AAge: Integer; const ABreed: string);
    procedure Speak; override;
    procedure Fetch;
    property Breed: string read FBreed write FBreed;
  end;

  TCat = class(TAnimal)
  private
    FIndoor: Boolean;
  public
    procedure Speak; override;
    procedure Purr;
    property Indoor: Boolean read FIndoor write FIndoor;
  end;

  { Interface type }
  IPrintable = interface
    ['{12345678-1234-1234-1234-123456789ABC}']
    function ToString: string;
    procedure Print;
  end;

  ISerializable = interface
    ['{87654321-4321-4321-4321-CBA987654321}']
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
  end;

  { Class implementing interfaces }
  TDocument = class(TInterfacedObject, IPrintable, ISerializable)
  private
    FContent: string;
  public
    function ToString: string; override;
    procedure Print;
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    property Content: string read FContent write FContent;
  end;

  { Generic class }
  generic TStack<T> = class
  private
    FItems: array of T;
    FCount: Integer;
  public
    constructor Create;
    procedure Push(const Item: T);
    function Pop: T;
    function Peek: T;
    function IsEmpty: Boolean;
    property Count: Integer read FCount;
  end;

  { Specialized generic }
  TIntStack = specialize TStack<Integer>;
  TStringStack = specialize TStack<string>;

  { Exception types }
  ECustomException = class(Exception);
  EValidationError = class(ECustomException);
  ENotFoundError = class(ECustomException);

{==============================================================================}
{ Global Variables                                                             }
{==============================================================================}

var
  GlobalCounter: Integer;
  GlobalBuffer: array[0..1023] of Byte;
  GlobalPerson: TPerson;
  GlobalList: TStringList;

{==============================================================================}
{ Record Methods Implementation                                                }
{==============================================================================}

class function TVector.Create(AX, AY, AZ: Double): TVector;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Z := AZ;
end;

function TVector.Length: Double;
begin
  Result := Sqrt(X * X + Y * Y + Z * Z);
end;

function TVector.Normalize: TVector;
var
  Len: Double;
begin
  Len := Self.Length;
  if Len > 0 then
  begin
    Result.X := X / Len;
    Result.Y := Y / Len;
    Result.Z := Z / Len;
  end
  else
    Result := Self;
end;

class operator TVector.+(const A, B: TVector): TVector;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
end;

class operator TVector.*(const V: TVector; S: Double): TVector;
begin
  Result.X := V.X * S;
  Result.Y := V.Y * S;
  Result.Z := V.Z * S;
end;

{==============================================================================}
{ Class Implementations                                                        }
{==============================================================================}

{ TAnimal }

constructor TAnimal.Create(const AName: string; AAge: Integer);
begin
  inherited Create;
  FName := AName;
  FAge := AAge;
end;

destructor TAnimal.Destroy;
begin
  { Cleanup code here }
  inherited Destroy;
end;

procedure TAnimal.SetName(const Value: string);
begin
  if Value <> FName then
    FName := Value;
end;

function TAnimal.GetDescription: string;
begin
  Result := Format('%s, Age: %d', [FName, FAge]);
end;

{ TDog }

constructor TDog.Create(const AName: string; AAge: Integer; const ABreed: string);
begin
  inherited Create(AName, AAge);
  FBreed := ABreed;
end;

procedure TDog.Speak;
begin
  WriteLn(Name, ' says: Woof!');
end;

procedure TDog.Fetch;
begin
  WriteLn(Name, ' is fetching the ball.');
end;

{ TCat }

procedure TCat.Speak;
begin
  WriteLn(Name, ' says: Meow!');
end;

procedure TCat.Purr;
begin
  WriteLn(Name, ' is purring...');
end;

{ TDocument }

function TDocument.ToString: string;
begin
  Result := FContent;
end;

procedure TDocument.Print;
begin
  WriteLn(FContent);
end;

procedure TDocument.SaveToStream(Stream: TStream);
var
  Len: Integer;
begin
  Len := Length(FContent);
  Stream.Write(Len, SizeOf(Len));
  if Len > 0 then
    Stream.Write(FContent[1], Len);
end;

procedure TDocument.LoadFromStream(Stream: TStream);
var
  Len: Integer;
begin
  Stream.Read(Len, SizeOf(Len));
  SetLength(FContent, Len);
  if Len > 0 then
    Stream.Read(FContent[1], Len);
end;

{ TStack }

constructor TStack.Create;
begin
  FCount := 0;
  SetLength(FItems, 16);
end;

procedure TStack.Push(const Item: T);
begin
  if FCount >= Length(FItems) then
    SetLength(FItems, Length(FItems) * 2);
  FItems[FCount] := Item;
  Inc(FCount);
end;

function TStack.Pop: T;
begin
  if FCount = 0 then
    raise ECustomException.Create('Stack is empty');
  Dec(FCount);
  Result := FItems[FCount];
end;

function TStack.Peek: T;
begin
  if FCount = 0 then
    raise ECustomException.Create('Stack is empty');
  Result := FItems[FCount - 1];
end;

function TStack.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

{==============================================================================}
{ Procedures and Functions                                                     }
{==============================================================================}

{ Simple procedure }
procedure SayHello;
begin
  WriteLn('Hello, Pascal!');
end;

{ Procedure with parameters }
procedure Greet(const Name: string; Times: Integer = 1);
var
  I: Integer;
begin
  for I := 1 to Times do
    WriteLn('Hello, ', Name, '!');
end;

{ Procedure with var parameters (pass by reference) }
procedure Swap(var A, B: Integer);
var
  Temp: Integer;
begin
  Temp := A;
  A := B;
  B := Temp;
end;

{ Procedure with out parameter }
procedure GetValues(out X, Y: Integer);
begin
  X := 10;
  Y := 20;
end;

{ Simple function }
function Add(A, B: Integer): Integer;
begin
  Result := A + B;
end;

{ Function with multiple exit points }
function Max(A, B: Integer): Integer;
begin
  if A > B then
    Exit(A)
  else
    Exit(B);
end;

{ Recursive function }
function Factorial(N: Integer): Int64;
begin
  if N <= 1 then
    Result := 1
  else
    Result := N * Factorial(N - 1);
end;

{ Nested procedures }
procedure OuterProcedure;
var
  OuterVar: Integer;

  procedure InnerProcedure;
  begin
    OuterVar := OuterVar + 1;
    WriteLn('Inner: ', OuterVar);
  end;

begin
  OuterVar := 10;
  InnerProcedure;
  WriteLn('Outer: ', OuterVar);
end;

{ Function with local functions }
function Calculate(X: Double): Double;

  function Square(N: Double): Double;
  begin
    Result := N * N;
  end;

  function Cube(N: Double): Double;
  begin
    Result := N * N * N;
  end;

begin
  Result := Square(X) + Cube(X);
end;

{ Inline function }
function Sqr(X: Double): Double; inline;
begin
  Result := X * X;
end;

{ Overloaded functions }
function ToString(Value: Integer): string; overload;
begin
  Result := IntToStr(Value);
end;

function ToString(Value: Double): string; overload;
begin
  Result := FloatToStr(Value);
end;

function ToString(Value: Boolean): string; overload;
begin
  if Value then
    Result := 'True'
  else
    Result := 'False';
end;

{ Generic function }
generic function Min<T>(A, B: T): T;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

{==============================================================================}
{ Main Program                                                                 }
{==============================================================================}

var
  { Local variables }
  I, J, K: Integer;
  X, Y, Z: Double;
  S: string;
  B: Boolean;
  C: Char;
  Day: TDayOfWeek;
  Person: TPerson;
  Point: TPoint;
  Dog: TDog;
  Cat: TCat;
  IntPtr: PInteger;
  DynArray: TIntArray;
  StringList: TStringList;
  Stack: TIntStack;
  V1, V2, V3: TVector;

begin
  WriteLn('=== Pascal Sample Program ===');
  WriteLn;

  { ===========================================================================
    Basic I/O
    =========================================================================== }

  Write('Enter your name: ');
  ReadLn(S);
  WriteLn('Hello, ', S, '!');

  { Formatted output }
  WriteLn('Integer: ', 42:10);
  WriteLn('Float: ', 3.14159:10:5);
  WriteLn('Boolean: ', True);

  { ===========================================================================
    Control Flow - If Statement
    =========================================================================== }

  I := 42;

  if I > 0 then
    WriteLn('Positive')
  else if I < 0 then
    WriteLn('Negative')
  else
    WriteLn('Zero');

  { Compound if }
  if (I > 0) and (I < 100) then
  begin
    WriteLn('Between 0 and 100');
    WriteLn('Value: ', I);
  end;

  { ===========================================================================
    Control Flow - Case Statement
    =========================================================================== }

  Day := dwMonday;

  case Day of
    dwSunday:
      WriteLn('Sunday - Rest day');
    dwMonday..dwFriday:
      WriteLn('Weekday - Work day');
    dwSaturday:
      WriteLn('Saturday - Fun day');
  else
    WriteLn('Unknown day');
  end;

  { Case with strings (Extended Pascal) }
  S := 'hello';
  case S of
    'hello': WriteLn('Greeting');
    'bye', 'goodbye': WriteLn('Farewell');
  else
    WriteLn('Unknown');
  end;

  { ===========================================================================
    Loops - For Loop
    =========================================================================== }

  { Counting up }
  for I := 1 to 10 do
    Write(I, ' ');
  WriteLn;

  { Counting down }
  for I := 10 downto 1 do
    Write(I, ' ');
  WriteLn;

  { For-in loop (FPC) }
  SetLength(DynArray, 5);
  for I := 0 to 4 do
    DynArray[I] := (I + 1) * 10;

  for I in DynArray do
    Write(I, ' ');
  WriteLn;

  { ===========================================================================
    Loops - While Loop
    =========================================================================== }

  I := 0;
  while I < 5 do
  begin
    WriteLn('While iteration: ', I);
    Inc(I);
  end;

  { ===========================================================================
    Loops - Repeat Until
    =========================================================================== }

  I := 0;
  repeat
    WriteLn('Repeat iteration: ', I);
    Inc(I);
  until I >= 5;

  { ===========================================================================
    Loops - Break and Continue
    =========================================================================== }

  for I := 1 to 100 do
  begin
    if I mod 2 = 0 then
      Continue;  { Skip even numbers }
    if I > 10 then
      Break;     { Exit loop }
    WriteLn('Odd: ', I);
  end;

  { ===========================================================================
    Exception Handling
    =========================================================================== }

  try
    I := 10;
    J := 0;
    { This would cause division by zero }
    { K := I div J; }

    raise EValidationError.Create('Test exception');
  except
    on E: EValidationError do
      WriteLn('Validation error: ', E.Message);
    on E: EDivByZero do
      WriteLn('Division by zero!');
    on E: Exception do
      WriteLn('General error: ', E.Message);
  end;

  { Try-finally }
  StringList := TStringList.Create;
  try
    StringList.Add('Item 1');
    StringList.Add('Item 2');
    for I := 0 to StringList.Count - 1 do
      WriteLn(StringList[I]);
  finally
    StringList.Free;
  end;

  { ===========================================================================
    Pointers and Dynamic Memory
    =========================================================================== }

  { Allocate memory }
  New(IntPtr);
  IntPtr^ := 42;
  WriteLn('Pointer value: ', IntPtr^);
  Dispose(IntPtr);

  { GetMem/FreeMem }
  IntPtr := GetMem(SizeOf(Integer));
  IntPtr^ := 100;
  WriteLn('GetMem value: ', IntPtr^);
  FreeMem(IntPtr);

  { ===========================================================================
    Dynamic Arrays
    =========================================================================== }

  SetLength(DynArray, 5);
  for I := 0 to High(DynArray) do
    DynArray[I] := I * I;

  WriteLn('Array length: ', Length(DynArray));
  WriteLn('Low: ', Low(DynArray), ', High: ', High(DynArray));

  { Resize }
  SetLength(DynArray, 10);

  { Free }
  SetLength(DynArray, 0);

  { ===========================================================================
    Records
    =========================================================================== }

  Person.FirstName := 'John';
  Person.LastName := 'Doe';
  Person.Age := 30;
  Person.BirthDate := EncodeDate(1994, 5, 15);
  Person.Active := True;

  with Person do
  begin
    WriteLn('Name: ', FirstName, ' ', LastName);
    WriteLn('Age: ', Age);
  end;

  { Advanced record }
  V1 := TVector.Create(1.0, 0.0, 0.0);
  V2 := TVector.Create(0.0, 1.0, 0.0);
  V3 := V1 + V2;
  WriteLn('Vector length: ', V3.Length:0:4);

  { ===========================================================================
    Objects (Classes)
    =========================================================================== }

  Dog := TDog.Create('Buddy', 5, 'Labrador');
  try
    Dog.Speak;
    Dog.Fetch;
    WriteLn('Description: ', Dog.Description);
  finally
    Dog.Free;
  end;

  Cat := TCat.Create('Whiskers', 3);
  try
    Cat.Indoor := True;
    Cat.Speak;
    Cat.Purr;
  finally
    Cat.Free;
  end;

  { ===========================================================================
    Generic Types
    =========================================================================== }

  Stack := TIntStack.Create;
  try
    Stack.Push(10);
    Stack.Push(20);
    Stack.Push(30);

    while not Stack.IsEmpty do
      WriteLn('Popped: ', Stack.Pop);
  finally
    Stack.Free;
  end;

  { ===========================================================================
    String Operations
    =========================================================================== }

  S := 'Hello, World!';

  WriteLn('Length: ', Length(S));
  WriteLn('Uppercase: ', UpperCase(S));
  WriteLn('Lowercase: ', LowerCase(S));
  WriteLn('Position of "World": ', Pos('World', S));
  WriteLn('Substring: ', Copy(S, 1, 5));
  WriteLn('Trimmed: "', Trim('  hello  '), '"');

  { String concatenation }
  S := 'Hello' + ' ' + 'World';
  S := Concat('Hello', ' ', 'World');

  { Format string }
  S := Format('Value: %d, Float: %.2f, String: %s', [42, 3.14, 'test']);
  WriteLn(S);

  { ===========================================================================
    Set Operations
    =========================================================================== }

  var
    CharSet: TCharSet;
    DaySet: TDaySet;

  CharSet := ['A', 'B', 'C'];

  if 'A' in CharSet then
    WriteLn('A is in set');

  Include(CharSet, 'D');
  Exclude(CharSet, 'A');

  DaySet := [dwMonday, dwWednesday, dwFriday];
  DaySet := DaySet + [dwTuesday];  { Union }
  DaySet := DaySet - [dwMonday];   { Difference }
  DaySet := DaySet * [dwTuesday, dwWednesday];  { Intersection }

  { ===========================================================================
    Math Operations
    =========================================================================== }

  X := 16.0;
  WriteLn('Sqrt: ', Sqrt(X):0:4);
  WriteLn('Power: ', Power(2, 10):0:0);
  WriteLn('Log: ', Ln(E):0:4);
  WriteLn('Sin: ', Sin(Pi/2):0:4);
  WriteLn('Cos: ', Cos(Pi):0:4);
  WriteLn('Abs: ', Abs(-42));
  WriteLn('Round: ', Round(3.7));
  WriteLn('Trunc: ', Trunc(3.7));
  WriteLn('Frac: ', Frac(3.7):0:4);
  WriteLn('Random: ', Random);

  { ===========================================================================
    Type Conversions
    =========================================================================== }

  I := 42;
  X := I;              { Implicit }
  X := Double(I);      { Explicit }

  S := IntToStr(I);
  I := StrToInt('123');
  I := StrToIntDef('abc', 0);  { With default }

  X := StrToFloat('3.14');
  S := FloatToStr(X);

  B := True;
  S := BoolToStr(B, True);

  { ===========================================================================
    File Operations
    =========================================================================== }

  var
    TextFile: Text;
    BinaryFile: File of Integer;
    Buffer: array[1..100] of Integer;

  { Text file }
  Assign(TextFile, 'test.txt');
  {$I-}
  Rewrite(TextFile);
  {$I+}
  if IOResult = 0 then
  begin
    WriteLn(TextFile, 'Line 1');
    WriteLn(TextFile, 'Line 2');
    Close(TextFile);
  end;

  { Binary file }
  Assign(BinaryFile, 'data.bin');
  {$I-}
  Rewrite(BinaryFile);
  {$I+}
  if IOResult = 0 then
  begin
    for I := 1 to 100 do
      Write(BinaryFile, I * 10);
    Close(BinaryFile);
  end;

  { ===========================================================================
    Date/Time
    =========================================================================== }

  var
    Now: TDateTime;
    Year, Month, Day: Word;
    Hour, Min, Sec, MSec: Word;

  Now := SysUtils.Now;
  DecodeDate(Now, Year, Month, Day);
  DecodeTime(Now, Hour, Min, Sec, MSec);

  WriteLn('Date: ', DateToStr(Now));
  WriteLn('Time: ', TimeToStr(Now));
  WriteLn('DateTime: ', DateTimeToStr(Now));
  WriteLn(Format('Formatted: %d-%d-%d %d:%d:%d',
                 [Year, Month, Day, Hour, Min, Sec]));

  { ===========================================================================
    Assembler (Inline)
    =========================================================================== }

  {$IFDEF CPUX86_64}
  asm
    mov rax, 42
    mov rbx, rax
  end;
  {$ENDIF}

  {$IFDEF CPUI386}
  asm
    mov eax, 42
    mov ebx, eax
  end;
  {$ENDIF}

  WriteLn;
  WriteLn('=== Program Complete ===');

end.

{==============================================================================}
{ Unit Example                                                                 }
{==============================================================================}

{
unit SampleUnit;

interface

uses
  Classes, SysUtils;

type
  TSampleClass = class
  public
    procedure DoSomething;
  end;

function PublicFunction(X: Integer): Integer;

implementation

function PrivateFunction(X: Integer): Integer;
begin
  Result := X * 2;
end;

function PublicFunction(X: Integer): Integer;
begin
  Result := PrivateFunction(X) + 1;
end;

procedure TSampleClass.DoSomething;
begin
  WriteLn('Doing something...');
end;

initialization
  WriteLn('Unit initialized');

finalization
  WriteLn('Unit finalized');

end.
}
