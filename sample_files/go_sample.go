// Package main demonstrates comprehensive Go language features
package main

import (
	"bufio"
	"bytes"
	"context"
	"crypto/sha256"
	"database/sql"
	"embed"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"log"
	"math"
	"net/http"
	"os"
	"os/signal"
	"reflect"
	"regexp"
	"runtime"
	"sort"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"
	"syscall"
	"time"
	"unsafe"
)

// Embed directive (Go 1.16+)
//go:embed sample.go
var embeddedFile embed.FS

// Constants
const (
	MaxBufferSize = 1024
	Pi            = 3.14159265358979323846
	AppName       = "SampleApp"
)

// Iota for enumeration
const (
	StatusOK = iota
	StatusError
	StatusPending
	StatusTimeout
)

// Bit flags with iota
const (
	FlagRead  = 1 << iota // 1
	FlagWrite             // 2
	FlagExec              // 4
)

// Type declarations
type (
	// Type alias
	ID = int64

	// Named type
	UserID int64

	// Function type
	Handler     func(ctx context.Context, req Request) (Response, error)
	Middleware  func(Handler) Handler
	Comparator  func(a, b interface{}) int
	Callback    func() error
)

// Struct types
type Point struct {
	X, Y, Z float64
}

type Person struct {
	Name      string    `json:"name" xml:"name" validate:"required"`
	Age       int       `json:"age,omitempty"`
	Email     string    `json:"email" db:"email_address"`
	CreatedAt time.Time `json:"created_at"`
	private   string    // unexported field
}

type Employee struct {
	Person             // Embedded struct
	EmployeeID  UserID `json:"employee_id"`
	Department  string `json:"department"`
	Salary      float64
	Manager     *Employee
	DirectReports []*Employee
}

// Generic struct (Go 1.18+)
type Container[T any] struct {
	items []T
	mu    sync.RWMutex
}

type Pair[K comparable, V any] struct {
	Key   K
	Value V
}

// Interface definitions
type Reader interface {
	Read(p []byte) (n int, err error)
}

type Writer interface {
	Write(p []byte) (n int, err error)
}

// Interface composition
type ReadWriter interface {
	Reader
	Writer
}

// Interface with type constraint (Go 1.18+)
type Ordered interface {
	~int | ~int8 | ~int16 | ~int32 | ~int64 |
		~uint | ~uint8 | ~uint16 | ~uint32 | ~uint64 |
		~float32 | ~float64 | ~string
}

type Number interface {
	~int | ~int32 | ~int64 | ~float32 | ~float64
}

// Stringer interface implementation
type Color int

const (
	Red Color = iota
	Green
	Blue
)

func (c Color) String() string {
	switch c {
	case Red:
		return "Red"
	case Green:
		return "Green"
	case Blue:
		return "Blue"
	default:
		return "Unknown"
	}
}

// Error interface implementation
type AppError struct {
	Code    int
	Message string
	Err     error
}

func (e *AppError) Error() string {
	if e.Err != nil {
		return fmt.Sprintf("[%d] %s: %v", e.Code, e.Message, e.Err)
	}
	return fmt.Sprintf("[%d] %s", e.Code, e.Message)
}

func (e *AppError) Unwrap() error {
	return e.Err
}

// Request/Response types
type Request struct {
	Method  string
	Path    string
	Headers map[string]string
	Body    []byte
}

type Response struct {
	StatusCode int
	Headers    map[string]string
	Body       []byte
}

// Methods on structs
func NewPoint(x, y, z float64) *Point {
	return &Point{X: x, Y: y, Z: z}
}

func (p Point) Distance(other Point) float64 {
	dx := p.X - other.X
	dy := p.Y - other.Y
	dz := p.Z - other.Z
	return math.Sqrt(dx*dx + dy*dy + dz*dz)
}

func (p *Point) Scale(factor float64) {
	p.X *= factor
	p.Y *= factor
	p.Z *= factor
}

func (p Point) Add(other Point) Point {
	return Point{
		X: p.X + other.X,
		Y: p.Y + other.Y,
		Z: p.Z + other.Z,
	}
}

// Generic functions (Go 1.18+)
func Min[T Ordered](a, b T) T {
	if a < b {
		return a
	}
	return b
}

func Max[T Ordered](a, b T) T {
	if a > b {
		return a
	}
	return b
}

func Map[T, U any](slice []T, f func(T) U) []U {
	result := make([]U, len(slice))
	for i, v := range slice {
		result[i] = f(v)
	}
	return result
}

func Filter[T any](slice []T, predicate func(T) bool) []T {
	result := make([]T, 0)
	for _, v := range slice {
		if predicate(v) {
			result = append(result, v)
		}
	}
	return result
}

func Reduce[T, U any](slice []T, initial U, f func(U, T) U) U {
	result := initial
	for _, v := range slice {
		result = f(result, v)
	}
	return result
}

// Generic container methods
func NewContainer[T any]() *Container[T] {
	return &Container[T]{
		items: make([]T, 0),
	}
}

func (c *Container[T]) Add(item T) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.items = append(c.items, item)
}

func (c *Container[T]) Get(index int) (T, bool) {
	c.mu.RLock()
	defer c.mu.RUnlock()
	var zero T
	if index < 0 || index >= len(c.items) {
		return zero, false
	}
	return c.items[index], true
}

func (c *Container[T]) Len() int {
	c.mu.RLock()
	defer c.mu.RUnlock()
	return len(c.items)
}

// Variadic functions
func Sum(numbers ...int) int {
	total := 0
	for _, n := range numbers {
		total += n
	}
	return total
}

func Printf(format string, args ...interface{}) {
	fmt.Printf(format, args...)
}

// Multiple return values
func Divide(a, b float64) (float64, error) {
	if b == 0 {
		return 0, errors.New("division by zero")
	}
	return a / b, nil
}

// Named return values
func MinMax(numbers []int) (min, max int, err error) {
	if len(numbers) == 0 {
		err = errors.New("empty slice")
		return
	}
	min, max = numbers[0], numbers[0]
	for _, n := range numbers[1:] {
		if n < min {
			min = n
		}
		if n > max {
			max = n
		}
	}
	return
}

// Defer, panic, recover
func SafeOperation() (err error) {
	defer func() {
		if r := recover(); r != nil {
			err = fmt.Errorf("panic recovered: %v", r)
		}
	}()

	// Deferred functions executed in LIFO order
	defer fmt.Println("Third")
	defer fmt.Println("Second")
	defer fmt.Println("First")

	// This would panic
	// panic("something went wrong")

	return nil
}

// Closures
func Counter() func() int {
	count := 0
	return func() int {
		count++
		return count
	}
}

func MakeMultiplier(factor int) func(int) int {
	return func(x int) int {
		return x * factor
	}
}

// Higher-order functions
func Apply(f func(int) int, values []int) []int {
	result := make([]int, len(values))
	for i, v := range values {
		result[i] = f(v)
	}
	return result
}

// Goroutines and channels
func worker(id int, jobs <-chan int, results chan<- int) {
	for job := range jobs {
		fmt.Printf("Worker %d processing job %d\n", id, job)
		time.Sleep(time.Millisecond * 100)
		results <- job * 2
	}
}

func fanOut(ctx context.Context, input <-chan int, workers int) []<-chan int {
	outputs := make([]<-chan int, workers)
	for i := 0; i < workers; i++ {
		out := make(chan int)
		outputs[i] = out
		go func(ch chan<- int) {
			defer close(ch)
			for {
				select {
				case <-ctx.Done():
					return
				case v, ok := <-input:
					if !ok {
						return
					}
					ch <- v * 2
				}
			}
		}(out)
	}
	return outputs
}

func fanIn(ctx context.Context, inputs ...<-chan int) <-chan int {
	output := make(chan int)
	var wg sync.WaitGroup

	for _, ch := range inputs {
		wg.Add(1)
		go func(c <-chan int) {
			defer wg.Done()
			for {
				select {
				case <-ctx.Done():
					return
				case v, ok := <-c:
					if !ok {
						return
					}
					output <- v
				}
			}
		}(ch)
	}

	go func() {
		wg.Wait()
		close(output)
	}()

	return output
}

// Select statement
func selectExample(ch1, ch2 <-chan int, quit <-chan struct{}) {
	for {
		select {
		case v1 := <-ch1:
			fmt.Println("Received from ch1:", v1)
		case v2 := <-ch2:
			fmt.Println("Received from ch2:", v2)
		case <-quit:
			fmt.Println("Quitting")
			return
		case <-time.After(time.Second):
			fmt.Println("Timeout")
		default:
			fmt.Println("No activity")
			time.Sleep(time.Millisecond * 100)
		}
	}
}

// Sync primitives
type SafeCounter struct {
	mu    sync.RWMutex
	value int
}

func (c *SafeCounter) Increment() {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.value++
}

func (c *SafeCounter) Value() int {
	c.mu.RLock()
	defer c.mu.RUnlock()
	return c.value
}

// Atomic operations
type AtomicCounter struct {
	value int64
}

func (c *AtomicCounter) Increment() {
	atomic.AddInt64(&c.value, 1)
}

func (c *AtomicCounter) Value() int64 {
	return atomic.LoadInt64(&c.value)
}

// Once
var (
	instance *singleton
	once     sync.Once
)

type singleton struct {
	data string
}

func GetInstance() *singleton {
	once.Do(func() {
		instance = &singleton{data: "initialized"}
	})
	return instance
}

// WaitGroup example
func parallelTasks(tasks []func()) {
	var wg sync.WaitGroup
	for _, task := range tasks {
		wg.Add(1)
		go func(t func()) {
			defer wg.Done()
			t()
		}(task)
	}
	wg.Wait()
}

// Pool example
var bufferPool = sync.Pool{
	New: func() interface{} {
		return new(bytes.Buffer)
	},
}

func usePool() {
	buf := bufferPool.Get().(*bytes.Buffer)
	defer func() {
		buf.Reset()
		bufferPool.Put(buf)
	}()
	buf.WriteString("Hello, Pool!")
}

// Context usage
func longOperation(ctx context.Context) error {
	select {
	case <-time.After(5 * time.Second):
		return nil
	case <-ctx.Done():
		return ctx.Err()
	}
}

func withTimeout() {
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()

	if err := longOperation(ctx); err != nil {
		fmt.Println("Operation cancelled:", err)
	}
}

func withCancel() {
	ctx, cancel := context.WithCancel(context.Background())

	go func() {
		time.Sleep(time.Second)
		cancel()
	}()

	<-ctx.Done()
	fmt.Println("Context cancelled")
}

// HTTP server
func httpServerExample() {
	mux := http.NewServeMux()

	// Handler function
	mux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "Hello, %s!", r.URL.Path[1:])
	})

	// Handler with closure
	mux.HandleFunc("/api/", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]string{
			"message": "Hello, API!",
		})
	})

	// Method-specific handling
	mux.HandleFunc("/resource", func(w http.ResponseWriter, r *http.Request) {
		switch r.Method {
		case http.MethodGet:
			fmt.Fprintln(w, "GET resource")
		case http.MethodPost:
			fmt.Fprintln(w, "POST resource")
		case http.MethodPut:
			fmt.Fprintln(w, "PUT resource")
		case http.MethodDelete:
			fmt.Fprintln(w, "DELETE resource")
		default:
			http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		}
	})

	server := &http.Server{
		Addr:         ":8080",
		Handler:      mux,
		ReadTimeout:  5 * time.Second,
		WriteTimeout: 10 * time.Second,
		IdleTimeout:  120 * time.Second,
	}

	// Graceful shutdown
	go func() {
		sigCh := make(chan os.Signal, 1)
		signal.Notify(sigCh, syscall.SIGINT, syscall.SIGTERM)
		<-sigCh

		ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
		defer cancel()

		server.Shutdown(ctx)
	}()

	// server.ListenAndServe()
}

// JSON handling
type Config struct {
	Host     string            `json:"host"`
	Port     int               `json:"port"`
	Debug    bool              `json:"debug"`
	Features []string          `json:"features"`
	Settings map[string]string `json:"settings"`
}

func jsonExample() {
	// Marshal
	config := Config{
		Host:     "localhost",
		Port:     8080,
		Debug:    true,
		Features: []string{"auth", "logging"},
		Settings: map[string]string{"key": "value"},
	}

	data, err := json.MarshalIndent(config, "", "  ")
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(string(data))

	// Unmarshal
	jsonStr := `{"host":"example.com","port":443}`
	var parsed Config
	if err := json.Unmarshal([]byte(jsonStr), &parsed); err != nil {
		log.Fatal(err)
	}

	// Streaming decode
	reader := strings.NewReader(jsonStr)
	decoder := json.NewDecoder(reader)
	if err := decoder.Decode(&parsed); err != nil {
		log.Fatal(err)
	}
}

// File I/O
func fileOperations() error {
	// Write file
	content := []byte("Hello, File!")
	if err := os.WriteFile("test.txt", content, 0644); err != nil {
		return err
	}

	// Read file
	data, err := os.ReadFile("test.txt")
	if err != nil {
		return err
	}
	fmt.Println(string(data))

	// Open with flags
	file, err := os.OpenFile("test.txt", os.O_RDWR|os.O_CREATE|os.O_APPEND, 0644)
	if err != nil {
		return err
	}
	defer file.Close()

	// Buffered I/O
	writer := bufio.NewWriter(file)
	writer.WriteString("Appended text\n")
	writer.Flush()

	// Buffered reading
	file.Seek(0, io.SeekStart)
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		fmt.Println(scanner.Text())
	}

	return scanner.Err()
}

// Reflection
func reflectionExample(v interface{}) {
	t := reflect.TypeOf(v)
	val := reflect.ValueOf(v)

	fmt.Println("Type:", t.Name())
	fmt.Println("Kind:", t.Kind())

	if t.Kind() == reflect.Struct {
		for i := 0; i < t.NumField(); i++ {
			field := t.Field(i)
			value := val.Field(i)
			fmt.Printf("Field: %s, Type: %s, Value: %v, Tag: %s\n",
				field.Name, field.Type, value, field.Tag.Get("json"))
		}
	}
}

// Type assertions and type switches
func typeAssertions(v interface{}) {
	// Type assertion
	if str, ok := v.(string); ok {
		fmt.Println("String:", str)
	}

	// Type switch
	switch x := v.(type) {
	case nil:
		fmt.Println("nil")
	case int:
		fmt.Println("int:", x)
	case string:
		fmt.Println("string:", x)
	case bool:
		fmt.Println("bool:", x)
	case []int:
		fmt.Println("[]int:", x)
	case map[string]int:
		fmt.Println("map[string]int:", x)
	default:
		fmt.Printf("unknown type: %T\n", x)
	}
}

// Unsafe operations
func unsafeExample() {
	// Get size and alignment
	var x int64
	fmt.Println("Size:", unsafe.Sizeof(x))
	fmt.Println("Align:", unsafe.Alignof(x))

	// Pointer conversion
	var i int32 = 42
	ptr := unsafe.Pointer(&i)
	floatPtr := (*float32)(ptr)
	_ = floatPtr

	// Struct field offset
	type Example struct {
		a int8
		b int64
		c int8
	}
	fmt.Println("Offset of b:", unsafe.Offsetof(Example{}.b))
}

// Regular expressions
func regexExample() {
	// Compile regex
	re := regexp.MustCompile(`(\w+)@(\w+)\.(\w+)`)

	// Match
	email := "user@example.com"
	if re.MatchString(email) {
		fmt.Println("Valid email")
	}

	// Find submatch
	matches := re.FindStringSubmatch(email)
	fmt.Println("User:", matches[1])
	fmt.Println("Domain:", matches[2])
	fmt.Println("TLD:", matches[3])

	// Replace
	result := re.ReplaceAllString(email, "${1}@redacted.${3}")
	fmt.Println("Redacted:", result)

	// Find all
	text := "Contact: a@b.com and c@d.org"
	allMatches := re.FindAllString(text, -1)
	fmt.Println("All emails:", allMatches)
}

// Sorting
func sortingExample() {
	// Sort slice
	numbers := []int{5, 2, 8, 1, 9}
	sort.Ints(numbers)

	strings := []string{"banana", "apple", "cherry"}
	sort.Strings(strings)

	// Custom sort
	people := []Person{
		{Name: "Alice", Age: 30},
		{Name: "Bob", Age: 25},
		{Name: "Charlie", Age: 35},
	}

	sort.Slice(people, func(i, j int) bool {
		return people[i].Age < people[j].Age
	})

	// Stable sort
	sort.SliceStable(people, func(i, j int) bool {
		return people[i].Name < people[j].Name
	})

	// Check if sorted
	isSorted := sort.IntsAreSorted(numbers)
	fmt.Println("Is sorted:", isSorted)

	// Binary search
	index := sort.SearchInts(numbers, 5)
	fmt.Println("Index of 5:", index)
}

// Main function
func main() {
	// Variable declarations
	var a int
	var b, c int = 1, 2
	d := 3
	e := "string"
	f := true
	g := 3.14
	h := 'A'
	i := []int{1, 2, 3}
	j := map[string]int{"one": 1}
	k := struct{ name string }{"anonymous"}

	// Zero values
	var (
		zeroInt    int
		zeroFloat  float64
		zeroBool   bool
		zeroString string
		zeroPtr    *int
		zeroSlice  []int
		zeroMap    map[string]int
		zeroChan   chan int
		zeroFunc   func()
	)

	_, _, _, _, _, _, _, _, _ = zeroInt, zeroFloat, zeroBool, zeroString, zeroPtr, zeroSlice, zeroMap, zeroChan, zeroFunc

	// Constants
	const (
		local    = "local"
		computed = 1 << 10
	)

	// Composite literals
	point := Point{X: 1, Y: 2, Z: 3}
	point2 := Point{1, 2, 3}
	pointPtr := &Point{X: 1, Y: 2}

	// Make and new
	slice := make([]int, 0, 10)
	channel := make(chan int, 100)
	mapVar := make(map[string]int)
	pointer := new(int)

	// Append and copy
	slice = append(slice, 1, 2, 3)
	slice = append(slice, []int{4, 5, 6}...)
	copied := make([]int, len(slice))
	copy(copied, slice)

	// Delete from map
	mapVar["key"] = 42
	delete(mapVar, "key")

	// Close channel
	close(channel)

	// Len and cap
	fmt.Println("Length:", len(slice))
	fmt.Println("Capacity:", cap(slice))

	// Control flow
	if x := 10; x > 5 {
		fmt.Println("x is greater than 5")
	} else if x > 0 {
		fmt.Println("x is positive")
	} else {
		fmt.Println("x is non-positive")
	}

	// For loop variations
	for i := 0; i < 10; i++ {
		fmt.Println(i)
	}

	for _, v := range slice {
		fmt.Println(v)
	}

	for key, value := range mapVar {
		fmt.Println(key, value)
	}

	// While-style loop
	count := 0
	for count < 10 {
		count++
	}

	// Infinite loop
	// for {
	//     break
	// }

	// Switch
	switch day := time.Now().Weekday(); day {
	case time.Saturday, time.Sunday:
		fmt.Println("Weekend")
	case time.Monday:
		fmt.Println("Monday")
		fallthrough
	default:
		fmt.Println("Weekday")
	}

	// Type switch
	typeAssertions("hello")
	typeAssertions(42)

	// Goroutine
	go func() {
		fmt.Println("Hello from goroutine")
	}()

	// Channel operations
	ch := make(chan int, 1)
	ch <- 42
	value := <-ch

	// Method calls
	p := NewPoint(1, 2, 3)
	distance := p.Distance(Point{4, 5, 6})
	p.Scale(2)

	// Generic function calls
	minVal := Min(1, 2)
	maxVal := Max("a", "b")
	mapped := Map([]int{1, 2, 3}, func(x int) string {
		return strconv.Itoa(x)
	})

	// Closure
	counter := Counter()
	fmt.Println(counter()) // 1
	fmt.Println(counter()) // 2

	// Error handling
	result, err := Divide(10, 2)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println("Result:", result)

	// Errors package
	if errors.Is(err, sql.ErrNoRows) {
		fmt.Println("No rows found")
	}

	var appErr *AppError
	if errors.As(err, &appErr) {
		fmt.Println("App error:", appErr.Code)
	}

	// Defer
	defer fmt.Println("Deferred")

	// Panic and recover
	SafeOperation()

	// Print runtime info
	fmt.Println("GOOS:", runtime.GOOS)
	fmt.Println("GOARCH:", runtime.GOARCH)
	fmt.Println("NumCPU:", runtime.NumCPU())
	fmt.Println("NumGoroutine:", runtime.NumGoroutine())

	// Crypto
	hash := sha256.Sum256([]byte("hello"))
	fmt.Printf("SHA256: %x\n", hash)

	// Unused variable suppression
	_, _, _, _, _, _, _, _, _, _ = a, b, c, d, e, f, g, h, i, j
	_ = k
	_ = point
	_ = point2
	_ = pointPtr
	_ = pointer
	_ = value
	_ = distance
	_ = minVal
	_ = maxVal
	_ = mapped

	fmt.Println("Program completed successfully!")
}

// Init function
func init() {
	fmt.Println("Package initialized")
}

// Build constraints
// +build linux darwin
// +build amd64
