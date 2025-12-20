/**
 * Comprehensive React (TSX) Sample - Syntax Highlighting Demonstration
 *
 * This file demonstrates all major React with TypeScript language features
 * for syntax highlighting purposes.
 */

import React, {
  useState,
  useEffect,
  useContext,
  useReducer,
  useCallback,
  useMemo,
  useRef,
  useImperativeHandle,
  forwardRef,
  memo,
  lazy,
  Suspense,
  createContext,
  Fragment,
  type ReactNode,
  type FC,
  type ComponentType,
  type MouseEvent,
  type ChangeEvent,
  type FormEvent,
  type KeyboardEvent,
  type RefObject,
  type MutableRefObject,
  type Dispatch,
  type SetStateAction,
  type PropsWithChildren,
  type HTMLAttributes,
  type ButtonHTMLAttributes,
  type InputHTMLAttributes,
} from "react";
import { createPortal } from "react-dom";

// =============================================================================
// Type Definitions
// =============================================================================

// Basic types
type Status = "idle" | "loading" | "success" | "error";
type Theme = "light" | "dark" | "system";
type Size = "sm" | "md" | "lg" | "xl";

// Interface for props
interface User {
  id: number;
  name: string;
  email: string;
  avatar?: string;
  role: "admin" | "user" | "guest";
  createdAt: Date;
}

interface ApiResponse<T> {
  data: T;
  status: number;
  message?: string;
}

// Props interfaces
interface ButtonProps extends ButtonHTMLAttributes<HTMLButtonElement> {
  variant?: "primary" | "secondary" | "outline" | "ghost";
  size?: Size;
  isLoading?: boolean;
  leftIcon?: ReactNode;
  rightIcon?: ReactNode;
}

interface InputProps extends Omit<InputHTMLAttributes<HTMLInputElement>, "size"> {
  label?: string;
  error?: string;
  helperText?: string;
  size?: Size;
}

interface CardProps extends HTMLAttributes<HTMLDivElement> {
  title?: string;
  subtitle?: string;
  footer?: ReactNode;
  isHoverable?: boolean;
  isPressable?: boolean;
}

interface ModalProps {
  isOpen: boolean;
  onClose: () => void;
  title?: string;
  children: ReactNode;
  size?: Size;
  closeOnOverlay?: boolean;
  closeOnEscape?: boolean;
}

interface ListProps<T> {
  items: T[];
  renderItem: (item: T, index: number) => ReactNode;
  keyExtractor: (item: T, index: number) => string | number;
  emptyState?: ReactNode;
  loading?: boolean;
  error?: Error | null;
}

// Discriminated union for reducer actions
type CounterAction =
  | { type: "INCREMENT" }
  | { type: "DECREMENT" }
  | { type: "SET"; payload: number }
  | { type: "RESET" };

interface CounterState {
  count: number;
  history: number[];
}

// Generic types
type Nullable<T> = T | null;
type Optional<T> = T | undefined;
type AsyncState<T> = {
  data: T | null;
  loading: boolean;
  error: Error | null;
};

// Utility types
type ComponentProps<T extends keyof JSX.IntrinsicElements | ComponentType<any>> =
  T extends ComponentType<infer P>
    ? P
    : T extends keyof JSX.IntrinsicElements
    ? JSX.IntrinsicElements[T]
    : never;

type ExtractProps<T> = T extends FC<infer P> ? P : never;

// =============================================================================
// Context
// =============================================================================

interface ThemeContextValue {
  theme: Theme;
  setTheme: Dispatch<SetStateAction<Theme>>;
  toggleTheme: () => void;
}

const ThemeContext = createContext<ThemeContextValue | null>(null);

interface UserContextValue {
  user: User | null;
  setUser: Dispatch<SetStateAction<User | null>>;
  logout: () => void;
}

const UserContext = createContext<UserContextValue | null>(null);

// Context provider with TypeScript
function ThemeProvider({ children }: PropsWithChildren): JSX.Element {
  const [theme, setTheme] = useState<Theme>("light");

  const value = useMemo<ThemeContextValue>(
    () => ({
      theme,
      setTheme,
      toggleTheme: () =>
        setTheme((prev) => (prev === "light" ? "dark" : "light")),
    }),
    [theme]
  );

  return (
    <ThemeContext.Provider value={value}>{children}</ThemeContext.Provider>
  );
}

// Type-safe custom hook
function useTheme(): ThemeContextValue {
  const context = useContext(ThemeContext);
  if (!context) {
    throw new Error("useTheme must be used within a ThemeProvider");
  }
  return context;
}

function useUser(): UserContextValue {
  const context = useContext(UserContext);
  if (!context) {
    throw new Error("useUser must be used within a UserProvider");
  }
  return context;
}

// =============================================================================
// Reducer with TypeScript
// =============================================================================

const initialState: CounterState = {
  count: 0,
  history: [],
};

function counterReducer(
  state: CounterState,
  action: CounterAction
): CounterState {
  switch (action.type) {
    case "INCREMENT":
      return {
        count: state.count + 1,
        history: [...state.history, state.count + 1],
      };
    case "DECREMENT":
      return {
        count: state.count - 1,
        history: [...state.history, state.count - 1],
      };
    case "SET":
      return {
        count: action.payload,
        history: [...state.history, action.payload],
      };
    case "RESET":
      return initialState;
    default:
      // Exhaustiveness check
      const _exhaustive: never = action;
      return state;
  }
}

// =============================================================================
// Generic Components
// =============================================================================

// Generic list component
function List<T>({
  items,
  renderItem,
  keyExtractor,
  emptyState,
  loading,
  error,
}: ListProps<T>): JSX.Element {
  if (loading) {
    return <div className="loading">Loading...</div>;
  }

  if (error) {
    return <div className="error">Error: {error.message}</div>;
  }

  if (items.length === 0) {
    return <>{emptyState || <div className="empty">No items</div>}</>;
  }

  return (
    <ul className="list">
      {items.map((item, index) => (
        <li key={keyExtractor(item, index)}>{renderItem(item, index)}</li>
      ))}
    </ul>
  );
}

// Generic select component
interface SelectOption<T> {
  value: T;
  label: string;
  disabled?: boolean;
}

interface SelectProps<T extends string | number> {
  options: SelectOption<T>[];
  value: T;
  onChange: (value: T) => void;
  placeholder?: string;
  disabled?: boolean;
}

function Select<T extends string | number>({
  options,
  value,
  onChange,
  placeholder,
  disabled,
}: SelectProps<T>): JSX.Element {
  const handleChange = (event: ChangeEvent<HTMLSelectElement>) => {
    const newValue = event.target.value as T;
    onChange(newValue);
  };

  return (
    <select value={value} onChange={handleChange} disabled={disabled}>
      {placeholder && (
        <option value="" disabled>
          {placeholder}
        </option>
      )}
      {options.map((option) => (
        <option
          key={String(option.value)}
          value={option.value}
          disabled={option.disabled}
        >
          {option.label}
        </option>
      ))}
    </select>
  );
}

// =============================================================================
// forwardRef with TypeScript
// =============================================================================

interface ForwardRefInputProps extends InputHTMLAttributes<HTMLInputElement> {
  label?: string;
  error?: string;
}

interface InputRef {
  focus: () => void;
  blur: () => void;
  clear: () => void;
  getValue: () => string;
}

const ForwardRefInput = forwardRef<HTMLInputElement, ForwardRefInputProps>(
  function ForwardRefInput({ label, error, ...props }, ref) {
    const id = React.useId();

    return (
      <div className="input-wrapper">
        {label && <label htmlFor={id}>{label}</label>}
        <input
          id={id}
          ref={ref}
          {...props}
          aria-invalid={!!error}
          aria-describedby={error ? `${id}-error` : undefined}
        />
        {error && (
          <span id={`${id}-error`} className="error" role="alert">
            {error}
          </span>
        )}
      </div>
    );
  }
);

// useImperativeHandle with TypeScript
const ImperativeInput = forwardRef<InputRef, InputHTMLAttributes<HTMLInputElement>>(
  function ImperativeInput(props, ref) {
    const inputRef = useRef<HTMLInputElement>(null);
    const [value, setValue] = useState("");

    useImperativeHandle(
      ref,
      (): InputRef => ({
        focus: () => inputRef.current?.focus(),
        blur: () => inputRef.current?.blur(),
        clear: () => setValue(""),
        getValue: () => value,
      }),
      [value]
    );

    return (
      <input
        ref={inputRef}
        value={value}
        onChange={(e) => setValue(e.target.value)}
        {...props}
      />
    );
  }
);

// =============================================================================
// Memo with TypeScript
// =============================================================================

interface MemoizedItemProps {
  id: number;
  name: string;
  onClick: (id: number) => void;
}

const MemoizedItem = memo<MemoizedItemProps>(
  function MemoizedItem({ id, name, onClick }) {
    console.log(`Rendering item ${id}`);
    return (
      <div onClick={() => onClick(id)} className="item">
        {name}
      </div>
    );
  },
  (prevProps, nextProps) =>
    prevProps.id === nextProps.id && prevProps.name === nextProps.name
);

// =============================================================================
// Custom Hooks with TypeScript
// =============================================================================

function useLocalStorage<T>(
  key: string,
  initialValue: T
): [T, Dispatch<SetStateAction<T>>] {
  const [storedValue, setStoredValue] = useState<T>(() => {
    try {
      const item = window.localStorage.getItem(key);
      return item ? (JSON.parse(item) as T) : initialValue;
    } catch (error) {
      console.error(error);
      return initialValue;
    }
  });

  const setValue: Dispatch<SetStateAction<T>> = useCallback(
    (value) => {
      try {
        const valueToStore =
          value instanceof Function ? value(storedValue) : value;
        setStoredValue(valueToStore);
        window.localStorage.setItem(key, JSON.stringify(valueToStore));
      } catch (error) {
        console.error(error);
      }
    },
    [key, storedValue]
  );

  return [storedValue, setValue];
}

function useFetch<T>(url: string): AsyncState<T> {
  const [state, setState] = useState<AsyncState<T>>({
    data: null,
    loading: true,
    error: null,
  });

  useEffect(() => {
    let isMounted = true;
    const controller = new AbortController();

    async function fetchData(): Promise<void> {
      try {
        setState((prev) => ({ ...prev, loading: true }));
        const response = await fetch(url, { signal: controller.signal });
        if (!response.ok) {
          throw new Error(`HTTP error! status: ${response.status}`);
        }
        const json = (await response.json()) as T;
        if (isMounted) {
          setState({ data: json, loading: false, error: null });
        }
      } catch (err) {
        if (isMounted && err instanceof Error && err.name !== "AbortError") {
          setState({ data: null, loading: false, error: err });
        }
      }
    }

    fetchData();

    return () => {
      isMounted = false;
      controller.abort();
    };
  }, [url]);

  return state;
}

function useDebounce<T>(value: T, delay: number): T {
  const [debouncedValue, setDebouncedValue] = useState<T>(value);

  useEffect(() => {
    const timer = setTimeout(() => {
      setDebouncedValue(value);
    }, delay);

    return () => {
      clearTimeout(timer);
    };
  }, [value, delay]);

  return debouncedValue;
}

function usePrevious<T>(value: T): T | undefined {
  const ref = useRef<T>();
  useEffect(() => {
    ref.current = value;
  });
  return ref.current;
}

function useToggle(initialValue = false): [boolean, () => void] {
  const [value, setValue] = useState(initialValue);
  const toggle = useCallback(() => setValue((v) => !v), []);
  return [value, toggle];
}

function useCounter(initialValue = 0): {
  count: number;
  increment: () => void;
  decrement: () => void;
  reset: () => void;
  set: (value: number) => void;
} {
  const [count, setCount] = useState(initialValue);

  return {
    count,
    increment: useCallback(() => setCount((c) => c + 1), []),
    decrement: useCallback(() => setCount((c) => c - 1), []),
    reset: useCallback(() => setCount(initialValue), [initialValue]),
    set: setCount,
  };
}

// =============================================================================
// Higher-Order Components with TypeScript
// =============================================================================

interface WithLoadingProps {
  isLoading: boolean;
}

function withLoading<P extends object>(
  WrappedComponent: ComponentType<P>
): FC<P & WithLoadingProps> {
  return function WithLoadingComponent({
    isLoading,
    ...props
  }: P & WithLoadingProps) {
    if (isLoading) {
      return <div className="loading">Loading...</div>;
    }
    return <WrappedComponent {...(props as P)} />;
  };
}

interface WithAuthProps {
  isAuthenticated: boolean;
}

function withAuth<P extends object>(
  WrappedComponent: ComponentType<P>,
  FallbackComponent: ComponentType = () => <div>Please login</div>
): FC<P & WithAuthProps> {
  return function WithAuthComponent({
    isAuthenticated,
    ...props
  }: P & WithAuthProps) {
    if (!isAuthenticated) {
      return <FallbackComponent />;
    }
    return <WrappedComponent {...(props as P)} />;
  };
}

// =============================================================================
// Render Props Pattern with TypeScript
// =============================================================================

interface RenderPropsChildren<T> {
  (props: T): ReactNode;
}

interface MousePosition {
  x: number;
  y: number;
}

interface MouseTrackerProps {
  render: RenderPropsChildren<MousePosition>;
}

function MouseTracker({ render }: MouseTrackerProps): JSX.Element {
  const [position, setPosition] = useState<MousePosition>({ x: 0, y: 0 });

  useEffect(() => {
    const handleMouseMove = (event: globalThis.MouseEvent) => {
      setPosition({ x: event.clientX, y: event.clientY });
    };

    window.addEventListener("mousemove", handleMouseMove);
    return () => window.removeEventListener("mousemove", handleMouseMove);
  }, []);

  return <>{render(position)}</>;
}

// Alternative with children as function
interface ChildrenAsFunctionProps<T> {
  children: RenderPropsChildren<T>;
}

function DataFetcher<T>({
  url,
  children,
}: { url: string } & ChildrenAsFunctionProps<AsyncState<T>>): JSX.Element {
  const state = useFetch<T>(url);
  return <>{children(state)}</>;
}

// =============================================================================
// Compound Components with TypeScript
// =============================================================================

interface TabsContextValue {
  activeTab: string;
  setActiveTab: (tab: string) => void;
}

const TabsContext = createContext<TabsContextValue | null>(null);

interface TabsProps {
  defaultTab: string;
  children: ReactNode;
  onChange?: (tab: string) => void;
}

function Tabs({ defaultTab, children, onChange }: TabsProps): JSX.Element {
  const [activeTab, setActiveTab] = useState(defaultTab);

  const handleSetActiveTab = useCallback(
    (tab: string) => {
      setActiveTab(tab);
      onChange?.(tab);
    },
    [onChange]
  );

  return (
    <TabsContext.Provider value={{ activeTab, setActiveTab: handleSetActiveTab }}>
      <div className="tabs">{children}</div>
    </TabsContext.Provider>
  );
}

interface TabListProps {
  children: ReactNode;
}

function TabList({ children }: TabListProps): JSX.Element {
  return (
    <div className="tab-list" role="tablist">
      {children}
    </div>
  );
}

interface TabProps {
  id: string;
  children: ReactNode;
  disabled?: boolean;
}

function Tab({ id, children, disabled }: TabProps): JSX.Element {
  const context = useContext(TabsContext);
  if (!context) {
    throw new Error("Tab must be used within Tabs");
  }

  const { activeTab, setActiveTab } = context;
  const isActive = activeTab === id;

  return (
    <button
      role="tab"
      aria-selected={isActive}
      aria-controls={`panel-${id}`}
      id={`tab-${id}`}
      className={`tab ${isActive ? "active" : ""}`}
      onClick={() => setActiveTab(id)}
      disabled={disabled}
    >
      {children}
    </button>
  );
}

interface TabPanelProps {
  id: string;
  children: ReactNode;
}

function TabPanel({ id, children }: TabPanelProps): JSX.Element | null {
  const context = useContext(TabsContext);
  if (!context) {
    throw new Error("TabPanel must be used within Tabs");
  }

  const { activeTab } = context;
  if (activeTab !== id) return null;

  return (
    <div
      role="tabpanel"
      id={`panel-${id}`}
      aria-labelledby={`tab-${id}`}
      className="tab-panel"
    >
      {children}
    </div>
  );
}

// Attach sub-components
Tabs.List = TabList;
Tabs.Tab = Tab;
Tabs.Panel = TabPanel;

// =============================================================================
// Event Handlers with TypeScript
// =============================================================================

function EventHandlerExamples(): JSX.Element {
  // Click events
  const handleClick = (event: MouseEvent<HTMLButtonElement>) => {
    event.preventDefault();
    console.log("Button clicked", event.currentTarget);
  };

  const handleDivClick = (event: MouseEvent<HTMLDivElement>) => {
    console.log("Div clicked", event.clientX, event.clientY);
  };

  // Change events
  const handleInputChange = (event: ChangeEvent<HTMLInputElement>) => {
    console.log("Input value:", event.target.value);
  };

  const handleSelectChange = (event: ChangeEvent<HTMLSelectElement>) => {
    console.log("Selected:", event.target.value);
  };

  const handleTextareaChange = (event: ChangeEvent<HTMLTextAreaElement>) => {
    console.log("Textarea:", event.target.value);
  };

  // Form events
  const handleSubmit = (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    const formData = new FormData(event.currentTarget);
    console.log("Form data:", Object.fromEntries(formData));
  };

  // Keyboard events
  const handleKeyDown = (event: KeyboardEvent<HTMLInputElement>) => {
    if (event.key === "Enter") {
      console.log("Enter pressed");
    }
    if (event.key === "Escape") {
      event.currentTarget.blur();
    }
  };

  // Focus events
  const handleFocus = (event: React.FocusEvent<HTMLInputElement>) => {
    console.log("Focused:", event.target.name);
  };

  const handleBlur = (event: React.FocusEvent<HTMLInputElement>) => {
    console.log("Blurred:", event.target.name);
  };

  return (
    <form onSubmit={handleSubmit}>
      <div onClick={handleDivClick}>
        <button type="button" onClick={handleClick}>
          Click me
        </button>
      </div>

      <input
        type="text"
        name="username"
        onChange={handleInputChange}
        onKeyDown={handleKeyDown}
        onFocus={handleFocus}
        onBlur={handleBlur}
      />

      <select onChange={handleSelectChange}>
        <option value="a">A</option>
        <option value="b">B</option>
      </select>

      <textarea onChange={handleTextareaChange} />

      <button type="submit">Submit</button>
    </form>
  );
}

// =============================================================================
// Utility Components
// =============================================================================

// Polymorphic component
type AsProps<C extends React.ElementType> = {
  as?: C;
};

type PolymorphicProps<C extends React.ElementType, P = {}> = AsProps<C> &
  Omit<React.ComponentPropsWithoutRef<C>, keyof AsProps<C> | keyof P> &
  P;

interface BoxOwnProps {
  padding?: Size;
  margin?: Size;
}

type BoxProps<C extends React.ElementType = "div"> = PolymorphicProps<
  C,
  BoxOwnProps
>;

function Box<C extends React.ElementType = "div">({
  as,
  padding,
  margin,
  className,
  ...props
}: BoxProps<C>): JSX.Element {
  const Component = as || "div";
  const classes = [className, padding && `p-${padding}`, margin && `m-${margin}`]
    .filter(Boolean)
    .join(" ");

  return <Component className={classes} {...props} />;
}

// Conditional render component
interface ShowProps {
  when: boolean;
  fallback?: ReactNode;
  children: ReactNode;
}

function Show({ when, fallback = null, children }: ShowProps): ReactNode {
  return when ? children : fallback;
}

// For each component
interface ForProps<T> {
  each: T[];
  children: (item: T, index: number) => ReactNode;
  fallback?: ReactNode;
}

function For<T>({ each, children, fallback }: ForProps<T>): ReactNode {
  if (each.length === 0) {
    return fallback;
  }
  return <>{each.map((item, index) => children(item, index))}</>;
}

// =============================================================================
// Main App Component
// =============================================================================

interface AppProps {
  initialUser?: User;
}

function App({ initialUser }: AppProps): JSX.Element {
  const [state, dispatch] = useReducer(counterReducer, initialState);
  const [users, setUsers] = useState<User[]>([]);
  const [selectedUserId, setSelectedUserId] = useState<number | null>(null);
  const inputRef = useRef<HTMLInputElement>(null);
  const imperativeRef = useRef<InputRef>(null);

  const { theme, toggleTheme } = useTheme();
  const { data, loading, error } = useFetch<User[]>("/api/users");
  const [searchTerm, setSearchTerm] = useLocalStorage("search", "");
  const debouncedSearch = useDebounce(searchTerm, 300);

  const filteredUsers = useMemo(() => {
    if (!debouncedSearch) return users;
    return users.filter((user) =>
      user.name.toLowerCase().includes(debouncedSearch.toLowerCase())
    );
  }, [users, debouncedSearch]);

  const handleUserClick = useCallback((id: number) => {
    setSelectedUserId(id);
  }, []);

  useEffect(() => {
    if (data) {
      setUsers(data);
    }
  }, [data]);

  return (
    <div className={`app theme-${theme}`}>
      <header>
        <h1>TypeScript React Sample</h1>
        <button onClick={toggleTheme}>Toggle Theme</button>
      </header>

      <main>
        {/* Counter with reducer */}
        <section>
          <h2>Counter: {state.count}</h2>
          <button onClick={() => dispatch({ type: "INCREMENT" })}>+</button>
          <button onClick={() => dispatch({ type: "DECREMENT" })}>-</button>
          <button onClick={() => dispatch({ type: "RESET" })}>Reset</button>
        </section>

        {/* Generic list */}
        <section>
          <input
            type="search"
            value={searchTerm}
            onChange={(e) => setSearchTerm(e.target.value)}
            placeholder="Search users..."
          />

          <List
            items={filteredUsers}
            keyExtractor={(user) => user.id}
            renderItem={(user) => (
              <MemoizedItem
                id={user.id}
                name={user.name}
                onClick={handleUserClick}
              />
            )}
            loading={loading}
            error={error}
            emptyState={<p>No users found</p>}
          />
        </section>

        {/* forwardRef components */}
        <section>
          <ForwardRefInput
            ref={inputRef}
            label="Email"
            type="email"
            placeholder="Enter email"
          />
          <button onClick={() => inputRef.current?.focus()}>Focus</button>
        </section>

        {/* Compound components */}
        <Tabs defaultTab="tab1" onChange={(tab) => console.log(tab)}>
          <Tabs.List>
            <Tabs.Tab id="tab1">Tab 1</Tabs.Tab>
            <Tabs.Tab id="tab2">Tab 2</Tabs.Tab>
            <Tabs.Tab id="tab3" disabled>
              Tab 3
            </Tabs.Tab>
          </Tabs.List>
          <Tabs.Panel id="tab1">Content 1</Tabs.Panel>
          <Tabs.Panel id="tab2">Content 2</Tabs.Panel>
          <Tabs.Panel id="tab3">Content 3</Tabs.Panel>
        </Tabs>

        {/* Render props */}
        <MouseTracker
          render={({ x, y }) => (
            <p>
              Mouse: {x}, {y}
            </p>
          )}
        />

        {/* Utility components */}
        <Show when={selectedUserId !== null} fallback={<p>Select a user</p>}>
          <p>Selected user: {selectedUserId}</p>
        </Show>

        <For each={[1, 2, 3]} fallback={<p>Empty</p>}>
          {(item, index) => (
            <div key={index}>
              Item {item} at index {index}
            </div>
          )}
        </For>

        {/* Polymorphic box */}
        <Box as="section" padding="md" margin="lg">
          <p>Box content</p>
        </Box>

        <Box as="article" padding="sm">
          <p>Article content</p>
        </Box>
      </main>
    </div>
  );
}

export default App;
