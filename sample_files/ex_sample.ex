# ==============================================================================
# Comprehensive Elixir Sample - Syntax Highlighting Demonstration
# ==============================================================================

defmodule Sample do
  @moduledoc """
  A comprehensive Elixir module demonstrating all major language features
  for syntax highlighting purposes.

  ## Features Covered
  - Data types and literals
  - Pattern matching
  - Functions and guards
  - Modules and structs
  - Protocols and behaviours
  - Macros and metaprogramming
  - Concurrency with processes
  - OTP patterns
  """

  # Module attributes
  @vsn "1.0.0"
  @author "Sample Author"
  @compile {:inline, add: 2}
  @deprecated "Use new_function/1 instead"
  @doc false
  @spec_attribute :custom_value

  # Type specifications
  @type status :: :active | :inactive | :pending
  @type user_id :: pos_integer()
  @type result :: {:ok, term()} | {:error, String.t()}
  @type callback_result :: {:reply, term(), state} | {:noreply, state}
  @typep state :: map()

  # Opaque type
  @opaque internal_state :: %{counter: integer(), data: list()}

  # ============================================================================
  # Numeric Literals
  # ============================================================================

  # Integer literals
  @integer_decimal 42
  @integer_negative -17
  @integer_hex 0xFF
  @integer_octal 0o755
  @integer_binary 0b101010
  @integer_with_underscores 1_000_000

  # Float literals
  @float_simple 3.14159
  @float_scientific 6.022e23
  @float_negative -2.71828

  # ============================================================================
  # String and Character Literals
  # ============================================================================

  # Strings (binaries)
  @string_simple "Hello, Elixir!"
  @string_interpolated "The answer is #{40 + 2}"
  @string_escaped "Line 1\nLine 2\tTabbed"
  @string_unicode "Hello, 世界! λ → ∞"

  # Heredoc strings
  @heredoc_string """
  This is a heredoc string.
  It can span multiple lines.
  Interpolation works: #{1 + 1}
  """

  # Sigils
  @sigil_string ~s(String with "quotes")
  @sigil_charlist ~c(charlist)
  @sigil_words ~w(one two three)a
  @sigil_regex ~r/pattern\s+\w+/i
  @sigil_date ~D[2024-01-15]
  @sigil_time ~T[14:30:00]
  @sigil_datetime ~U[2024-01-15 14:30:00Z]
  @sigil_naive_datetime ~N[2024-01-15 14:30:00]

  # Custom sigil
  @sigil_custom ~S|raw string without interpolation #{this stays literal}|

  # Charlists
  @charlist 'Hello'
  @charlist_escaped 'Line\nBreak'

  # Atoms
  @atom_simple :hello
  @atom_with_spaces :"hello world"
  @atom_unicode :λ
  @atom_special :@
  @atom_module Sample

  # ============================================================================
  # Data Structures
  # ============================================================================

  # Lists
  @list_simple [1, 2, 3, 4, 5]
  @list_mixed [1, :two, "three", 4.0]
  @list_nested [[1, 2], [3, 4]]
  @list_charlist [104, 101, 108, 108, 111]  # 'hello'

  # Keyword lists
  @keyword_list [name: "Alice", age: 30, active: true]

  # Tuples
  @tuple_pair {:ok, "success"}
  @tuple_triple {:error, 404, "Not Found"}

  # Maps
  @map_atom_keys %{name: "Alice", age: 30}
  @map_string_keys %{"name" => "Alice", "age" => 30}
  @map_mixed %{:atom_key => 1, "string_key" => 2}

  # Structs are defined separately
  defmodule User do
    @enforce_keys [:name, :email]
    defstruct [:id, :name, :email, :age, active: true, roles: []]

    @type t :: %__MODULE__{
      id: pos_integer() | nil,
      name: String.t(),
      email: String.t(),
      age: non_neg_integer() | nil,
      active: boolean(),
      roles: [atom()]
    }
  end

  # Ranges
  @range_inclusive 1..10
  @range_exclusive 1..10//1
  @range_step 0..100//5

  # ============================================================================
  # Pattern Matching
  # ============================================================================

  @doc "Pattern matching examples"
  def pattern_matching_examples do
    # Simple matching
    x = 1
    1 = x

    # Tuple matching
    {:ok, result} = {:ok, "success"}

    # List matching
    [head | tail] = [1, 2, 3, 4]
    [first, second | rest] = [1, 2, 3, 4]
    [a, b, c] = [1, 2, 3]

    # Map matching
    %{name: name} = %{name: "Alice", age: 30}
    %{name: n, age: a} = %{name: "Bob", age: 25}

    # Pin operator
    expected = "hello"
    ^expected = "hello"

    # Matching in function heads (see functions below)
    {result, head, tail, first, second, rest, a, b, c, name, n}
  end

  # ============================================================================
  # Functions
  # ============================================================================

  @doc """
  Adds two numbers together.

  ## Examples

      iex> Sample.add(2, 3)
      5

  """
  @spec add(number(), number()) :: number()
  def add(a, b), do: a + b

  # Function with guards
  @spec factorial(non_neg_integer()) :: pos_integer()
  def factorial(n) when is_integer(n) and n >= 0 do
    do_factorial(n, 1)
  end

  defp do_factorial(0, acc), do: acc
  defp do_factorial(n, acc), do: do_factorial(n - 1, n * acc)

  # Multiple function clauses with pattern matching
  def describe(:active), do: "Currently active"
  def describe(:inactive), do: "Not active"
  def describe(:pending), do: "Awaiting activation"
  def describe(other), do: "Unknown status: #{inspect(other)}"

  # Guards with multiple conditions
  def classify(n) when is_integer(n) and n < 0, do: :negative
  def classify(0), do: :zero
  def classify(n) when is_integer(n) and n > 0, do: :positive
  def classify(n) when is_float(n), do: :float
  def classify(_), do: :unknown

  # Default arguments
  def greet(name, greeting \\ "Hello") do
    "#{greeting}, #{name}!"
  end

  # Anonymous functions
  def higher_order_examples do
    # Lambda syntax
    double = fn x -> x * 2 end
    double.(5)

    # Capture syntax
    triple = &(&1 * 3)
    triple.(5)

    # Capturing named functions
    add_fn = &add/2
    add_fn.(1, 2)

    # Multi-clause anonymous function
    status_fn = fn
      :ok -> "Success"
      :error -> "Failure"
      _ -> "Unknown"
    end

    status_fn.(:ok)
  end

  # Private function
  defp internal_helper(x) do
    x * 2
  end

  # Function with keyword options
  def create_user(name, opts \\ []) do
    age = Keyword.get(opts, :age, 0)
    email = Keyword.get(opts, :email, "")
    active = Keyword.get(opts, :active, true)

    %User{name: name, email: email, age: age, active: active}
  end

  # ============================================================================
  # Control Flow
  # ============================================================================

  def control_flow_examples(value) do
    # if/else
    result1 =
      if value > 0 do
        :positive
      else
        :non_positive
      end

    # unless
    result2 =
      unless value == 0 do
        :non_zero
      end

    # case
    result3 =
      case value do
        0 -> :zero
        n when n > 0 -> :positive
        n when n < 0 -> :negative
        _ -> :unknown
      end

    # cond
    result4 =
      cond do
        value < 0 -> :negative
        value == 0 -> :zero
        value < 10 -> :small
        value < 100 -> :medium
        true -> :large
      end

    # with for chaining
    result5 =
      with {:ok, a} <- {:ok, 1},
           {:ok, b} <- {:ok, 2},
           true <- a + b == 3 do
        {:ok, a + b}
      else
        {:error, reason} -> {:error, reason}
        false -> {:error, :validation_failed}
      end

    {result1, result2, result3, result4, result5}
  end

  # ============================================================================
  # Comprehensions
  # ============================================================================

  def comprehension_examples do
    # Simple for comprehension
    squares = for n <- 1..10, do: n * n

    # With filter
    even_squares = for n <- 1..10, rem(n, 2) == 0, do: n * n

    # Multiple generators
    pairs = for x <- 1..3, y <- 1..3, do: {x, y}

    # With into
    map = for {k, v} <- [a: 1, b: 2], into: %{}, do: {k, v * 2}

    # Bitstring generator
    bytes = for <<byte <- "hello">>, do: byte

    # Nested with pattern matching
    nested = for {:ok, value} <- [{:ok, 1}, {:error, 2}, {:ok, 3}], do: value

    {squares, even_squares, pairs, map, bytes, nested}
  end

  # ============================================================================
  # Processes and Concurrency
  # ============================================================================

  def spawn_process do
    # Spawn a new process
    pid = spawn(fn ->
      receive do
        {:hello, sender} ->
          send(sender, {:world, self()})
        :quit ->
          :ok
      after
        5000 -> :timeout
      end
    end)

    pid
  end

  def linked_process do
    # Spawn and link
    spawn_link(fn ->
      Process.sleep(1000)
      raise "Intentional crash"
    end)
  end

  def monitored_process do
    # Spawn and monitor
    {pid, ref} = spawn_monitor(fn ->
      Process.sleep(1000)
      :done
    end)

    receive do
      {:DOWN, ^ref, :process, ^pid, reason} ->
        {:completed, reason}
    after
      5000 -> :timeout
    end
  end

  # ============================================================================
  # Protocols
  # ============================================================================

  defprotocol Stringify do
    @doc "Converts a value to a string representation"
    @spec to_str(t) :: String.t()
    def to_str(value)
  end

  defimpl Stringify, for: Integer do
    def to_str(value), do: "Integer: #{value}"
  end

  defimpl Stringify, for: Atom do
    def to_str(value), do: "Atom: #{value}"
  end

  defimpl Stringify, for: User do
    def to_str(%User{name: name, email: email}) do
      "User: #{name} <#{email}>"
    end
  end

  # Derive protocol implementation
  defmodule Product do
    @derive {Inspect, only: [:name, :price]}
    defstruct [:id, :name, :price, :secret]
  end

  # ============================================================================
  # Behaviours
  # ============================================================================

  defmodule Parser do
    @callback parse(String.t()) :: {:ok, term()} | {:error, String.t()}
    @callback format(term()) :: String.t()
    @optional_callbacks format: 1
  end

  defmodule JSONParser do
    @behaviour Parser

    @impl Parser
    def parse(string) do
      # Simplified JSON parsing
      {:ok, string}
    end

    @impl Parser
    def format(data) do
      inspect(data)
    end
  end

  # ============================================================================
  # GenServer Example
  # ============================================================================

  defmodule Counter do
    use GenServer

    # Client API
    def start_link(initial_value \\ 0) do
      GenServer.start_link(__MODULE__, initial_value, name: __MODULE__)
    end

    def increment do
      GenServer.call(__MODULE__, :increment)
    end

    def decrement do
      GenServer.call(__MODULE__, :decrement)
    end

    def get_value do
      GenServer.call(__MODULE__, :get)
    end

    def reset(value \\ 0) do
      GenServer.cast(__MODULE__, {:reset, value})
    end

    # Server Callbacks
    @impl GenServer
    def init(initial_value) do
      {:ok, initial_value}
    end

    @impl GenServer
    def handle_call(:increment, _from, state) do
      {:reply, state + 1, state + 1}
    end

    @impl GenServer
    def handle_call(:decrement, _from, state) do
      {:reply, state - 1, state - 1}
    end

    @impl GenServer
    def handle_call(:get, _from, state) do
      {:reply, state, state}
    end

    @impl GenServer
    def handle_cast({:reset, value}, _state) do
      {:noreply, value}
    end

    @impl GenServer
    def handle_info(:tick, state) do
      IO.puts("Current value: #{state}")
      Process.send_after(self(), :tick, 1000)
      {:noreply, state}
    end

    @impl GenServer
    def terminate(reason, state) do
      IO.puts("Terminating with reason: #{inspect(reason)}, state: #{state}")
      :ok
    end
  end

  # ============================================================================
  # Macros and Metaprogramming
  # ============================================================================

  defmacro debug(expr) do
    quote do
      result = unquote(expr)
      IO.puts("#{unquote(Macro.to_string(expr))} = #{inspect(result)}")
      result
    end
  end

  defmacro unless_macro(condition, do: block) do
    quote do
      if !unquote(condition) do
        unquote(block)
      end
    end
  end

  defmacro define_getter(name, value) do
    quote do
      def unquote(name)() do
        unquote(value)
      end
    end
  end

  # Using the macro
  define_getter(:app_name, "Sample Application")
  define_getter(:version, @vsn)

  # AST manipulation
  def ast_example do
    # Quote
    quoted = quote do
      1 + 2
    end
    # => {:+, [context: Elixir, import: Kernel], [1, 2]}

    # Unquote fragments
    number = 5
    quoted_with_value = quote do
      unquote(number) * 2
    end

    {quoted, quoted_with_value}
  end

  # ============================================================================
  # Exception Handling
  # ============================================================================

  defmodule CustomError do
    defexception [:message, :code]

    @impl true
    def exception(opts) do
      message = Keyword.get(opts, :message, "An error occurred")
      code = Keyword.get(opts, :code, 500)
      %__MODULE__{message: message, code: code}
    end
  end

  def exception_examples do
    # Try/rescue
    result1 = try do
      raise "Something went wrong"
    rescue
      e in RuntimeError -> {:error, e.message}
      _ -> {:error, :unknown}
    end

    # Try/catch
    result2 = try do
      throw(:some_value)
    catch
      :throw, value -> {:caught, value}
      :exit, reason -> {:exit, reason}
      :error, reason -> {:error, reason}
    end

    # Try/after
    result3 = try do
      :ok
    after
      IO.puts("Cleanup code runs here")
    end

    # Reraise
    try do
      raise "Original error"
    rescue
      e ->
        IO.puts("Logging: #{Exception.message(e)}")
        reraise e, __STACKTRACE__
    end

    {result1, result2, result3}
  end

  # ============================================================================
  # Streams and Lazy Evaluation
  # ============================================================================

  def stream_examples do
    # Create streams
    stream1 = Stream.iterate(0, &(&1 + 1))
    stream2 = Stream.cycle([1, 2, 3])
    stream3 = Stream.unfold(0, fn n -> {n, n + 1} end)

    # Stream transformations
    result1 = stream1
    |> Stream.map(&(&1 * 2))
    |> Stream.filter(&(rem(&1, 4) == 0))
    |> Stream.take(10)
    |> Enum.to_list()

    # File streaming
    # Stream.resource(
    #   fn -> File.open!("file.txt") end,
    #   fn file ->
    #     case IO.read(file, :line) do
    #       :eof -> {:halt, file}
    #       line -> {[line], file}
    #     end
    #   end,
    #   fn file -> File.close(file) end
    # )

    # Async streams (requires Task)
    tasks = for i <- 1..5 do
      Task.async(fn -> i * 2 end)
    end
    result2 = Task.await_many(tasks)

    {result1, result2}
  end

  # ============================================================================
  # Binary Pattern Matching
  # ============================================================================

  def binary_examples do
    # Bitstring syntax
    <<a, b, c>> = "abc"
    <<head::binary-size(4), rest::binary>> = "Hello, World!"

    # Integer encoding
    <<int::32-big>> = <<0, 0, 1, 0>>
    <<int_little::32-little>> = <<0, 1, 0, 0>>

    # Float encoding
    <<float::64-float>> = <<64, 9, 33, 251, 84, 68, 45, 24>>

    # Building binaries
    packet = <<1::8, 100::16-big, "payload"::binary>>

    # Pattern matching in function heads
    {a, b, c, head, rest, int, int_little, packet}
  end

  def parse_packet(<<type::8, length::16-big, payload::binary-size(length), rest::binary>>) do
    {:ok, %{type: type, payload: payload}, rest}
  end
  def parse_packet(_), do: {:error, :invalid_packet}

  # ============================================================================
  # Pipe Operator and Function Composition
  # ============================================================================

  def pipeline_examples(data) do
    data
    |> Enum.filter(&(&1 > 0))
    |> Enum.map(&(&1 * 2))
    |> Enum.sort()
    |> Enum.take(5)
    |> Enum.sum()
  end

  # Function composition
  def compose(f, g) do
    fn x -> f.(g.(x)) end
  end

  # ============================================================================
  # Documentation
  # ============================================================================

  @doc """
  A well-documented function.

  ## Parameters

  - `input` - The input value to process
  - `opts` - Keyword list of options
    - `:format` - Output format (`:json` | `:text`)
    - `:validate` - Whether to validate input (default: `true`)

  ## Returns

  Returns `{:ok, result}` on success or `{:error, reason}` on failure.

  ## Examples

      iex> Sample.documented_function("hello", format: :text)
      {:ok, "HELLO"}

      iex> Sample.documented_function("", validate: true)
      {:error, :empty_input}

  ## See Also

  - `other_function/1`
  - `another_function/2`
  """
  @spec documented_function(String.t(), keyword()) :: {:ok, String.t()} | {:error, atom()}
  def documented_function(input, opts \\ []) do
    format = Keyword.get(opts, :format, :text)
    validate = Keyword.get(opts, :validate, true)

    cond do
      validate and input == "" -> {:error, :empty_input}
      format == :text -> {:ok, String.upcase(input)}
      format == :json -> {:ok, ~s({"value": "#{input}"})}
      true -> {:error, :unknown_format}
    end
  end

  # ============================================================================
  # Module Attributes as Constants
  # ============================================================================

  @days_of_week ~w(monday tuesday wednesday thursday friday saturday sunday)a
  @max_retries 3
  @timeout_ms 5_000

  def get_day(index) when index >= 0 and index < 7 do
    Enum.at(@days_of_week, index)
  end

  # ============================================================================
  # Use, Import, Require, Alias
  # ============================================================================

  # These are typically at the top, but shown here for demonstration
  # use GenServer
  # import Enum, only: [map: 2, filter: 2]
  # require Logger
  # alias MyApp.Accounts.User, as: AccountUser

  def logging_example do
    require Logger
    Logger.info("This is an info message")
    Logger.warning("This is a warning")
    Logger.error("This is an error")
    Logger.debug("This is debug info")
  end
end

# ==============================================================================
# Top-level expressions
# ==============================================================================

# Module usage
alias Sample.User

# Creating a user
user = %User{
  id: 1,
  name: "Alice",
  email: "alice@example.com",
  age: 30,
  active: true,
  roles: [:admin, :user]
}

# Updating struct
updated_user = %{user | age: 31}

# Pattern matching on struct
%User{name: name, email: email} = user

IO.puts("=== Elixir Sample Program ===")
IO.puts("User: #{name} <#{email}>")
IO.puts("Factorial 10: #{Sample.factorial(10)}")
IO.puts("Done!")
