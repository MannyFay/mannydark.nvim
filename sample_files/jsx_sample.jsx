/**
 * Comprehensive React (JSX) Sample - Syntax Highlighting Demonstration
 *
 * This file demonstrates all major React and JSX language features
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
  useLayoutEffect,
  useDebugValue,
  forwardRef,
  memo,
  lazy,
  Suspense,
  createContext,
  Fragment,
  StrictMode,
  startTransition,
  useTransition,
  useDeferredValue,
  useId,
  useSyncExternalStore,
  useInsertionEffect,
} from "react";
import { createPortal } from "react-dom";
import PropTypes from "prop-types";

// =============================================================================
// Context
// =============================================================================

const ThemeContext = createContext("light");
const UserContext = createContext(null);
const DispatchContext = createContext(null);

// Context provider component
function ThemeProvider({ children, initialTheme = "light" }) {
  const [theme, setTheme] = useState(initialTheme);

  const value = useMemo(
    () => ({
      theme,
      setTheme,
      toggleTheme: () => setTheme((t) => (t === "light" ? "dark" : "light")),
    }),
    [theme]
  );

  return (
    <ThemeContext.Provider value={value}>{children}</ThemeContext.Provider>
  );
}

// Custom hook for context
function useTheme() {
  const context = useContext(ThemeContext);
  if (context === undefined) {
    throw new Error("useTheme must be used within a ThemeProvider");
  }
  return context;
}

// =============================================================================
// Reducer
// =============================================================================

const initialState = {
  count: 0,
  items: [],
  loading: false,
  error: null,
};

function reducer(state, action) {
  switch (action.type) {
    case "INCREMENT":
      return { ...state, count: state.count + 1 };
    case "DECREMENT":
      return { ...state, count: state.count - 1 };
    case "SET_COUNT":
      return { ...state, count: action.payload };
    case "ADD_ITEM":
      return { ...state, items: [...state.items, action.payload] };
    case "REMOVE_ITEM":
      return {
        ...state,
        items: state.items.filter((item) => item.id !== action.payload),
      };
    case "SET_LOADING":
      return { ...state, loading: action.payload };
    case "SET_ERROR":
      return { ...state, error: action.payload };
    case "RESET":
      return initialState;
    default:
      throw new Error(`Unknown action: ${action.type}`);
  }
}

// =============================================================================
// Class Component
// =============================================================================

class ClassComponent extends React.Component {
  // Static properties
  static displayName = "ClassComponent";
  static defaultProps = {
    title: "Default Title",
    count: 0,
  };

  // Instance properties
  state = {
    value: "",
    isOpen: false,
  };

  // Constructor
  constructor(props) {
    super(props);
    this.inputRef = React.createRef();
    this.handleChange = this.handleChange.bind(this);
  }

  // Lifecycle methods
  componentDidMount() {
    console.log("Component mounted");
    this.inputRef.current?.focus();
  }

  componentDidUpdate(prevProps, prevState) {
    if (prevState.value !== this.state.value) {
      console.log("Value changed:", this.state.value);
    }
  }

  componentWillUnmount() {
    console.log("Component will unmount");
  }

  shouldComponentUpdate(nextProps, nextState) {
    return (
      nextProps.count !== this.props.count ||
      nextState.value !== this.state.value
    );
  }

  getSnapshotBeforeUpdate(prevProps, prevState) {
    return { prevValue: prevState.value };
  }

  static getDerivedStateFromProps(props, state) {
    if (props.resetValue) {
      return { value: "" };
    }
    return null;
  }

  static getDerivedStateFromError(error) {
    return { hasError: true };
  }

  componentDidCatch(error, errorInfo) {
    console.error("Error caught:", error, errorInfo);
  }

  // Event handlers
  handleChange(event) {
    this.setState({ value: event.target.value });
  }

  handleToggle = () => {
    this.setState((prevState) => ({ isOpen: !prevState.isOpen }));
  };

  handleSubmit = (event) => {
    event.preventDefault();
    this.props.onSubmit?.(this.state.value);
  };

  // Render
  render() {
    const { title, count, children } = this.props;
    const { value, isOpen } = this.state;

    return (
      <div className="class-component">
        <h2>{title}</h2>
        <p>Count: {count}</p>

        <form onSubmit={this.handleSubmit}>
          <input
            ref={this.inputRef}
            type="text"
            value={value}
            onChange={this.handleChange}
            placeholder="Enter value"
          />
          <button type="submit">Submit</button>
        </form>

        <button onClick={this.handleToggle}>
          {isOpen ? "Close" : "Open"}
        </button>

        {isOpen && <div className="content">{children}</div>}
      </div>
    );
  }
}

ClassComponent.propTypes = {
  title: PropTypes.string,
  count: PropTypes.number,
  onSubmit: PropTypes.func,
  resetValue: PropTypes.bool,
  children: PropTypes.node,
};

// =============================================================================
// Function Component with Hooks
// =============================================================================

function FunctionComponent({
  initialValue = 0,
  items = [],
  onUpdate,
  className,
  style,
  ...restProps
}) {
  // useState
  const [count, setCount] = useState(initialValue);
  const [text, setText] = useState("");
  const [isVisible, setIsVisible] = useState(true);
  const [data, setData] = useState(null);

  // useReducer
  const [state, dispatch] = useReducer(reducer, initialState);

  // useRef
  const inputRef = useRef(null);
  const countRef = useRef(count);
  const prevCountRef = useRef();

  // useId
  const id = useId();

  // useTransition
  const [isPending, startTransition] = useTransition();

  // useDeferredValue
  const deferredText = useDeferredValue(text);

  // Custom hook
  const { theme } = useTheme();

  // useEffect
  useEffect(() => {
    console.log("Component mounted");

    const handleResize = () => {
      console.log("Window resized");
    };

    window.addEventListener("resize", handleResize);

    return () => {
      console.log("Cleanup");
      window.removeEventListener("resize", handleResize);
    };
  }, []);

  // useEffect with dependencies
  useEffect(() => {
    countRef.current = count;
    onUpdate?.(count);
  }, [count, onUpdate]);

  // useLayoutEffect
  useLayoutEffect(() => {
    prevCountRef.current = count;
  });

  // useCallback
  const handleIncrement = useCallback(() => {
    setCount((c) => c + 1);
  }, []);

  const handleDecrement = useCallback(() => {
    setCount((c) => c - 1);
  }, []);

  const handleChange = useCallback((event) => {
    const value = event.target.value;
    startTransition(() => {
      setText(value);
    });
  }, []);

  // useMemo
  const expensiveValue = useMemo(() => {
    console.log("Computing expensive value");
    return items.reduce((sum, item) => sum + item.value, 0);
  }, [items]);

  const doubledCount = useMemo(() => count * 2, [count]);

  // Event handlers
  const handleSubmit = (event) => {
    event.preventDefault();
    dispatch({ type: "ADD_ITEM", payload: { id: Date.now(), text } });
    setText("");
  };

  const handleReset = () => {
    setCount(initialValue);
    dispatch({ type: "RESET" });
  };

  // Conditional rendering
  if (!isVisible) {
    return null;
  }

  // JSX return
  return (
    <div
      className={`function-component ${className || ""}`}
      style={{ ...style, backgroundColor: theme === "dark" ? "#333" : "#fff" }}
      {...restProps}
    >
      {/* Heading */}
      <h2>Function Component</h2>

      {/* Counter */}
      <div className="counter">
        <button onClick={handleDecrement} aria-label="Decrement">
          -
        </button>
        <span id={`${id}-count`}>{count}</span>
        <button onClick={handleIncrement} aria-label="Increment">
          +
        </button>
      </div>

      {/* Form */}
      <form onSubmit={handleSubmit}>
        <label htmlFor={`${id}-input`}>Text:</label>
        <input
          id={`${id}-input`}
          ref={inputRef}
          type="text"
          value={text}
          onChange={handleChange}
          placeholder="Enter text"
        />
        <button type="submit" disabled={!text.trim()}>
          Add
        </button>
      </form>

      {/* Pending state */}
      {isPending && <span className="loading">Updating...</span>}

      {/* Computed values */}
      <p>Doubled: {doubledCount}</p>
      <p>Total: {expensiveValue}</p>
      <p>Deferred: {deferredText}</p>

      {/* List rendering */}
      <ul>
        {state.items.map((item) => (
          <li key={item.id}>
            {item.text}
            <button
              onClick={() =>
                dispatch({ type: "REMOVE_ITEM", payload: item.id })
              }
            >
              Remove
            </button>
          </li>
        ))}
      </ul>

      {/* Conditional rendering */}
      {state.items.length === 0 && <p>No items</p>}

      {/* Reset button */}
      <button onClick={handleReset}>Reset</button>

      {/* Toggle visibility */}
      <button onClick={() => setIsVisible(false)}>Hide</button>
    </div>
  );
}

FunctionComponent.propTypes = {
  initialValue: PropTypes.number,
  items: PropTypes.arrayOf(
    PropTypes.shape({
      id: PropTypes.number.isRequired,
      value: PropTypes.number.isRequired,
    })
  ),
  onUpdate: PropTypes.func,
  className: PropTypes.string,
  style: PropTypes.object,
};

// =============================================================================
// forwardRef Component
// =============================================================================

const ForwardRefInput = forwardRef(function ForwardRefInput(
  { label, error, ...props },
  ref
) {
  const id = useId();

  return (
    <div className="input-wrapper">
      <label htmlFor={id}>{label}</label>
      <input id={id} ref={ref} {...props} aria-invalid={!!error} />
      {error && (
        <span className="error" role="alert">
          {error}
        </span>
      )}
    </div>
  );
});

ForwardRefInput.displayName = "ForwardRefInput";

// =============================================================================
// useImperativeHandle
// =============================================================================

const ImperativeInput = forwardRef(function ImperativeInput(props, ref) {
  const inputRef = useRef(null);
  const [value, setValue] = useState("");

  useImperativeHandle(
    ref,
    () => ({
      focus: () => inputRef.current?.focus(),
      blur: () => inputRef.current?.blur(),
      clear: () => setValue(""),
      getValue: () => value,
      setValue: (newValue) => setValue(newValue),
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
});

// =============================================================================
// Memo Component
// =============================================================================

const MemoizedComponent = memo(
  function MemoizedComponent({ value, onClick }) {
    console.log("MemoizedComponent rendered");
    return (
      <div onClick={onClick}>
        <p>Value: {value}</p>
      </div>
    );
  },
  (prevProps, nextProps) => {
    return prevProps.value === nextProps.value;
  }
);

// =============================================================================
// Lazy Component
// =============================================================================

const LazyComponent = lazy(() => import("./LazyComponent"));

function LazyWrapper() {
  return (
    <Suspense fallback={<div>Loading...</div>}>
      <LazyComponent />
    </Suspense>
  );
}

// =============================================================================
// Error Boundary
// =============================================================================

class ErrorBoundary extends React.Component {
  state = { hasError: false, error: null };

  static getDerivedStateFromError(error) {
    return { hasError: true, error };
  }

  componentDidCatch(error, errorInfo) {
    console.error("Error caught by boundary:", error, errorInfo);
  }

  render() {
    if (this.state.hasError) {
      return (
        <div className="error-boundary">
          <h2>Something went wrong</h2>
          <details>
            <summary>Error details</summary>
            <pre>{this.state.error?.toString()}</pre>
          </details>
          <button onClick={() => this.setState({ hasError: false })}>
            Try again
          </button>
        </div>
      );
    }

    return this.props.children;
  }
}

// =============================================================================
// Portal Component
// =============================================================================

function Modal({ isOpen, onClose, children }) {
  if (!isOpen) return null;

  return createPortal(
    <div className="modal-overlay" onClick={onClose}>
      <div className="modal-content" onClick={(e) => e.stopPropagation()}>
        <button className="modal-close" onClick={onClose}>
          ×
        </button>
        {children}
      </div>
    </div>,
    document.getElementById("modal-root")
  );
}

// =============================================================================
// Custom Hooks
// =============================================================================

function useLocalStorage(key, initialValue) {
  const [storedValue, setStoredValue] = useState(() => {
    try {
      const item = window.localStorage.getItem(key);
      return item ? JSON.parse(item) : initialValue;
    } catch (error) {
      console.error(error);
      return initialValue;
    }
  });

  const setValue = useCallback(
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

function useFetch(url) {
  const [data, setData] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);

  useEffect(() => {
    let isMounted = true;
    const controller = new AbortController();

    async function fetchData() {
      try {
        setLoading(true);
        const response = await fetch(url, { signal: controller.signal });
        if (!response.ok) {
          throw new Error(`HTTP error! status: ${response.status}`);
        }
        const json = await response.json();
        if (isMounted) {
          setData(json);
          setError(null);
        }
      } catch (err) {
        if (isMounted && err.name !== "AbortError") {
          setError(err.message);
        }
      } finally {
        if (isMounted) {
          setLoading(false);
        }
      }
    }

    fetchData();

    return () => {
      isMounted = false;
      controller.abort();
    };
  }, [url]);

  return { data, loading, error };
}

function useDebounce(value, delay) {
  const [debouncedValue, setDebouncedValue] = useState(value);

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

function usePrevious(value) {
  const ref = useRef();
  useEffect(() => {
    ref.current = value;
  });
  return ref.current;
}

function useMediaQuery(query) {
  const [matches, setMatches] = useState(
    () => window.matchMedia(query).matches
  );

  useEffect(() => {
    const mediaQuery = window.matchMedia(query);
    const handler = (event) => setMatches(event.matches);

    mediaQuery.addEventListener("change", handler);
    return () => mediaQuery.removeEventListener("change", handler);
  }, [query]);

  return matches;
}

// =============================================================================
// Render Props Pattern
// =============================================================================

function MouseTracker({ render }) {
  const [position, setPosition] = useState({ x: 0, y: 0 });

  useEffect(() => {
    const handleMouseMove = (event) => {
      setPosition({ x: event.clientX, y: event.clientY });
    };

    window.addEventListener("mousemove", handleMouseMove);
    return () => window.removeEventListener("mousemove", handleMouseMove);
  }, []);

  return render(position);
}

// Usage:
// <MouseTracker render={({ x, y }) => <p>Mouse: {x}, {y}</p>} />

// =============================================================================
// Higher-Order Component
// =============================================================================

function withLoading(WrappedComponent) {
  return function WithLoadingComponent({ isLoading, ...props }) {
    if (isLoading) {
      return <div className="loading">Loading...</div>;
    }
    return <WrappedComponent {...props} />;
  };
}

// =============================================================================
// Compound Components
// =============================================================================

const Tabs = ({ children, defaultTab }) => {
  const [activeTab, setActiveTab] = useState(defaultTab);

  return (
    <TabsContext.Provider value={{ activeTab, setActiveTab }}>
      <div className="tabs">{children}</div>
    </TabsContext.Provider>
  );
};

const TabsContext = createContext();

Tabs.List = function TabsList({ children }) {
  return <div className="tabs-list">{children}</div>;
};

Tabs.Tab = function Tab({ id, children }) {
  const { activeTab, setActiveTab } = useContext(TabsContext);
  return (
    <button
      className={`tab ${activeTab === id ? "active" : ""}`}
      onClick={() => setActiveTab(id)}
    >
      {children}
    </button>
  );
};

Tabs.Panels = function TabsPanels({ children }) {
  return <div className="tabs-panels">{children}</div>;
};

Tabs.Panel = function TabsPanel({ id, children }) {
  const { activeTab } = useContext(TabsContext);
  if (activeTab !== id) return null;
  return <div className="tab-panel">{children}</div>;
};

// =============================================================================
// JSX Expressions and Patterns
// =============================================================================

function JSXPatterns({ items, user, config }) {
  // Conditional rendering patterns
  const conditionalElement = user ? <span>{user.name}</span> : null;
  const shortCircuit = user && <span>{user.name}</span>;
  const ternary = user ? <UserProfile user={user} /> : <LoginButton />;

  // Logical OR for defaults
  const displayName = user?.name || "Anonymous";

  // Fragment usage
  const fragmentExample = (
    <>
      <header>Header</header>
      <main>Main</main>
      <footer>Footer</footer>
    </>
  );

  // Keyed fragment
  const keyedFragment = items.map((item) => (
    <Fragment key={item.id}>
      <dt>{item.term}</dt>
      <dd>{item.definition}</dd>
    </Fragment>
  ));

  // Spread props
  const inputProps = {
    type: "text",
    placeholder: "Enter value",
    className: "input",
  };
  const spreadExample = <input {...inputProps} />;

  // Dynamic tag
  const Tag = config.useSection ? "section" : "div";
  const dynamicTag = <Tag className="container">Content</Tag>;

  // Children manipulation
  const childCount = React.Children.count(items);
  const mappedChildren = React.Children.map(items, (child, index) =>
    React.cloneElement(child, { index })
  );

  // Style object
  const styles = {
    container: {
      display: "flex",
      flexDirection: "column",
      gap: "1rem",
    },
    item: {
      padding: "0.5rem",
      backgroundColor: "#f0f0f0",
    },
  };

  // CSS Modules (simulated)
  const cssModuleStyles = {
    card: "Card_card__abc123",
    title: "Card_title__def456",
  };

  // Event handlers with arguments
  const handleItemClick = (id) => (event) => {
    event.preventDefault();
    console.log("Clicked item:", id);
  };

  return (
    <div style={styles.container}>
      {/* Various JSX patterns */}
      {conditionalElement}
      {shortCircuit}
      {ternary}
      <p>Hello, {displayName}!</p>

      {/* List rendering with index */}
      <ul>
        {items.map((item, index) => (
          <li
            key={item.id}
            style={styles.item}
            onClick={handleItemClick(item.id)}
            data-index={index}
          >
            {item.name}
          </li>
        ))}
      </ul>

      {/* Empty state */}
      {items.length === 0 && <p>No items found</p>}

      {/* Number coercion */}
      <p>Count: {items.length}</p>

      {/* Boolean attributes */}
      <input type="checkbox" checked={config.enabled} disabled={!user} readOnly />

      {/* Inline function (avoid in render for performance) */}
      <button onClick={() => console.log("Clicked")}>Click me</button>

      {/* Comments in JSX */}
      {/* This is a JSX comment */}

      {/* Dangerously set HTML (use with caution) */}
      <div dangerouslySetInnerHTML={{ __html: "<strong>Bold</strong>" }} />
    </div>
  );
}

// Placeholder components
const UserProfile = ({ user }) => <div>{user.name}</div>;
const LoginButton = () => <button>Login</button>;

// =============================================================================
// Main App Component
// =============================================================================

function App() {
  const [count, setCount] = useState(0);
  const [isModalOpen, setIsModalOpen] = useState(false);
  const imperativeRef = useRef();

  return (
    <StrictMode>
      <ThemeProvider>
        <ErrorBoundary>
          <div className="app">
            <h1>React JSX Sample</h1>

            {/* Function component */}
            <FunctionComponent
              initialValue={count}
              onUpdate={setCount}
            />

            {/* Class component */}
            <ClassComponent title="Class Example" count={count}>
              <p>Children content</p>
            </ClassComponent>

            {/* forwardRef usage */}
            <ForwardRefInput
              label="Email"
              type="email"
              placeholder="Enter email"
            />

            {/* Imperative handle usage */}
            <ImperativeInput ref={imperativeRef} placeholder="Imperative" />
            <button onClick={() => imperativeRef.current?.focus()}>
              Focus
            </button>

            {/* Modal */}
            <button onClick={() => setIsModalOpen(true)}>Open Modal</button>
            <Modal isOpen={isModalOpen} onClose={() => setIsModalOpen(false)}>
              <h2>Modal Title</h2>
              <p>Modal content</p>
            </Modal>

            {/* Lazy loaded component */}
            <LazyWrapper />

            {/* Tabs compound component */}
            <Tabs defaultTab="tab1">
              <Tabs.List>
                <Tabs.Tab id="tab1">Tab 1</Tabs.Tab>
                <Tabs.Tab id="tab2">Tab 2</Tabs.Tab>
                <Tabs.Tab id="tab3">Tab 3</Tabs.Tab>
              </Tabs.List>
              <Tabs.Panels>
                <Tabs.Panel id="tab1">Content 1</Tabs.Panel>
                <Tabs.Panel id="tab2">Content 2</Tabs.Panel>
                <Tabs.Panel id="tab3">Content 3</Tabs.Panel>
              </Tabs.Panels>
            </Tabs>

            {/* Mouse tracker render prop */}
            <MouseTracker
              render={({ x, y }) => (
                <p>
                  Mouse position: {x}, {y}
                </p>
              )}
            />
          </div>
        </ErrorBoundary>
      </ThemeProvider>
    </StrictMode>
  );
}

export default App;
