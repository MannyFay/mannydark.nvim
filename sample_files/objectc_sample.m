// ==============================================================================
// Comprehensive Objective-C Sample - Syntax Highlighting Demonstration
// ==============================================================================

// This file demonstrates all major Objective-C language features
// for syntax highlighting purposes.

#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

// ==============================================================================
// Comments
// ==============================================================================

// Single line comment

/* Multi-line
   block comment */

/// Documentation comment
/// @param name The name parameter
/// @return The greeting string

#pragma mark - Section Marker
#pragma mark Section Name

// ==============================================================================
// Preprocessor Directives
// ==============================================================================

#define MAX_SIZE 100
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define STRINGIFY(x) #x
#define CONCAT(a, b) a##b

#ifdef DEBUG
    #define LOG(fmt, ...) NSLog(fmt, ##__VA_ARGS__)
#else
    #define LOG(fmt, ...)
#endif

#ifndef CONSTANT
    #define CONSTANT 42
#endif

#if __IPHONE_OS_VERSION_MIN_REQUIRED >= 140000
    // iOS 14+ code
#endif

// ==============================================================================
// Forward Declarations
// ==============================================================================

@class Person;
@protocol Drawable;

// ==============================================================================
// C Types and Structures
// ==============================================================================

// Typedef
typedef int32_t MyInteger;
typedef void (^CompletionBlock)(BOOL success, NSError *error);
typedef NSString * (^StringTransformer)(NSString *input);

// Enum (C style)
typedef enum {
    StatusUnknown,
    StatusActive,
    StatusInactive
} Status;

// Enum (NS_ENUM)
typedef NS_ENUM(NSInteger, Direction) {
    DirectionNorth,
    DirectionSouth,
    DirectionEast,
    DirectionWest
};

// Options (NS_OPTIONS)
typedef NS_OPTIONS(NSUInteger, Permissions) {
    PermissionNone = 0,
    PermissionRead = 1 << 0,
    PermissionWrite = 1 << 1,
    PermissionExecute = 1 << 2,
    PermissionAll = PermissionRead | PermissionWrite | PermissionExecute
};

// Struct
struct Point {
    CGFloat x;
    CGFloat y;
};
typedef struct Point Point;

struct Rectangle {
    Point origin;
    CGFloat width;
    CGFloat height;
};

// Union
union Value {
    int intValue;
    float floatValue;
    char charValue;
};

// ==============================================================================
// Constants
// ==============================================================================

// String constants
NSString * const kAppName = @"Sample App";
NSString * const kNotificationName = @"com.example.notification";

// Numeric constants
static const NSInteger kMaxItems = 100;
static const CGFloat kDefaultPadding = 8.0f;

// Extern constants (declared here, defined elsewhere)
extern NSString * const kExternalConstant;

// ==============================================================================
// Protocol Declaration
// ==============================================================================

@protocol Drawable <NSObject>

@required
- (void)draw;
- (CGRect)bounds;

@optional
- (void)drawWithColor:(UIColor *)color;
- (void)animateWithDuration:(NSTimeInterval)duration;

@property (nonatomic, readonly) BOOL isVisible;

@end

@protocol DataSource <NSObject>

- (NSInteger)numberOfItems;
- (id)itemAtIndex:(NSInteger)index;

@end

@protocol Delegate <NSObject>

@optional
- (void)didSelectItemAtIndex:(NSInteger)index;
- (BOOL)shouldSelectItemAtIndex:(NSInteger)index;

@end

// ==============================================================================
// Class Interface (Header)
// ==============================================================================

@interface Person : NSObject <NSCopying, NSCoding, Drawable>
{
    // Instance variables (ivars)
    @private
    NSString *_secretCode;

    @protected
    NSInteger _internalCounter;

    @public
    BOOL publicFlag;

    @package
    NSString *_packageString;
}

// Properties
@property (nonatomic, strong) NSString *firstName;
@property (nonatomic, strong) NSString *lastName;
@property (nonatomic, assign) NSInteger age;
@property (nonatomic, readonly) NSString *fullName;
@property (nonatomic, copy) NSString *email;
@property (nonatomic, weak) id<Delegate> delegate;
@property (atomic, strong) NSMutableArray *items;
@property (nonatomic, getter=isActive) BOOL active;
@property (class, nonatomic, readonly) NSInteger instanceCount;

// Nullability
@property (nonatomic, strong, nullable) NSString *middleName;
@property (nonatomic, strong, nonnull) NSString *identifier;

// Class methods
+ (instancetype)person;
+ (instancetype)personWithName:(NSString *)name age:(NSInteger)age;
+ (NSArray<Person *> *)allPeople;

// Instance methods
- (instancetype)init NS_DESIGNATED_INITIALIZER;
- (instancetype)initWithName:(NSString *)name;
- (instancetype)initWithName:(NSString *)name age:(NSInteger)age;

- (void)sayHello;
- (NSString *)greetingForPerson:(Person *)other;
- (void)performActionWithCompletion:(CompletionBlock)completion;

// Method with multiple parameters
- (void)setFirstName:(NSString *)firstName
            lastName:(NSString *)lastName
                 age:(NSInteger)age;

// Variadic method
- (void)logMessages:(NSString *)firstMessage, ... NS_REQUIRES_NIL_TERMINATION;

@end

// ==============================================================================
// Class Extension (Private Interface)
// ==============================================================================

@interface Person ()

@property (nonatomic, strong) NSMutableArray *privateItems;
@property (nonatomic, assign) BOOL isInitialized;

- (void)privateSetup;
- (void)privateMethod;

@end

// ==============================================================================
// Class Implementation
// ==============================================================================

@implementation Person

// Synthesize
@synthesize firstName = _firstName;
@synthesize lastName = _lastName;

static NSInteger _instanceCount = 0;

#pragma mark - Lifecycle

+ (void)initialize {
    if (self == [Person class]) {
        // One-time class initialization
    }
}

+ (void)load {
    // Called when class is loaded
}

- (instancetype)init {
    self = [super init];
    if (self) {
        _privateItems = [NSMutableArray array];
        _isInitialized = YES;
        _instanceCount++;
        [self privateSetup];
    }
    return self;
}

- (instancetype)initWithName:(NSString *)name {
    return [self initWithName:name age:0];
}

- (instancetype)initWithName:(NSString *)name age:(NSInteger)age {
    self = [self init];
    if (self) {
        _firstName = [name copy];
        _age = age;
    }
    return self;
}

- (void)dealloc {
    _instanceCount--;
    [[NSNotificationCenter defaultCenter] removeObserver:self];
}

#pragma mark - Class Methods

+ (instancetype)person {
    return [[self alloc] init];
}

+ (instancetype)personWithName:(NSString *)name age:(NSInteger)age {
    return [[self alloc] initWithName:name age:age];
}

+ (NSArray<Person *> *)allPeople {
    return @[];
}

+ (NSInteger)instanceCount {
    return _instanceCount;
}

#pragma mark - Public Methods

- (void)sayHello {
    NSLog(@"Hello, my name is %@", self.fullName);
}

- (NSString *)greetingForPerson:(Person *)other {
    return [NSString stringWithFormat:@"Hello %@, I'm %@",
            other.firstName, self.firstName];
}

- (void)performActionWithCompletion:(CompletionBlock)completion {
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        BOOL success = YES;
        NSError *error = nil;

        dispatch_async(dispatch_get_main_queue(), ^{
            if (completion) {
                completion(success, error);
            }
        });
    });
}

- (void)setFirstName:(NSString *)firstName
            lastName:(NSString *)lastName
                 age:(NSInteger)age {
    self.firstName = firstName;
    self.lastName = lastName;
    self.age = age;
}

- (void)logMessages:(NSString *)firstMessage, ... {
    va_list args;
    va_start(args, firstMessage);

    NSString *message = firstMessage;
    while (message != nil) {
        NSLog(@"%@", message);
        message = va_arg(args, NSString *);
    }

    va_end(args);
}

#pragma mark - Private Methods

- (void)privateSetup {
    // Private setup code
}

- (void)privateMethod {
    // Private implementation
}

#pragma mark - Properties

- (NSString *)fullName {
    return [NSString stringWithFormat:@"%@ %@", self.firstName, self.lastName];
}

#pragma mark - NSCopying

- (id)copyWithZone:(NSZone *)zone {
    Person *copy = [[Person allocWithZone:zone] init];
    copy.firstName = [self.firstName copy];
    copy.lastName = [self.lastName copy];
    copy.age = self.age;
    return copy;
}

#pragma mark - NSCoding

- (void)encodeWithCoder:(NSCoder *)coder {
    [coder encodeObject:self.firstName forKey:@"firstName"];
    [coder encodeObject:self.lastName forKey:@"lastName"];
    [coder encodeInteger:self.age forKey:@"age"];
}

- (instancetype)initWithCoder:(NSCoder *)coder {
    self = [self init];
    if (self) {
        _firstName = [coder decodeObjectForKey:@"firstName"];
        _lastName = [coder decodeObjectForKey:@"lastName"];
        _age = [coder decodeIntegerForKey:@"age"];
    }
    return self;
}

#pragma mark - Drawable Protocol

- (void)draw {
    NSLog(@"Drawing person: %@", self.fullName);
}

- (CGRect)bounds {
    return CGRectMake(0, 0, 100, 100);
}

#pragma mark - NSObject

- (NSString *)description {
    return [NSString stringWithFormat:@"<%@: %p, name=%@, age=%ld>",
            NSStringFromClass([self class]), self, self.fullName, (long)self.age];
}

- (BOOL)isEqual:(id)object {
    if (self == object) return YES;
    if (![object isKindOfClass:[Person class]]) return NO;

    Person *other = (Person *)object;
    return [self.firstName isEqualToString:other.firstName] &&
           [self.lastName isEqualToString:other.lastName] &&
           self.age == other.age;
}

- (NSUInteger)hash {
    return self.firstName.hash ^ self.lastName.hash ^ self.age;
}

@end

// ==============================================================================
// Category
// ==============================================================================

@interface Person (Validation)

- (BOOL)isValidEmail;
- (BOOL)isAdult;

@end

@implementation Person (Validation)

- (BOOL)isValidEmail {
    NSString *pattern = @"[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}";
    NSPredicate *predicate = [NSPredicate predicateWithFormat:@"SELF MATCHES %@", pattern];
    return [predicate evaluateWithObject:self.email];
}

- (BOOL)isAdult {
    return self.age >= 18;
}

@end

// ==============================================================================
// Subclass
// ==============================================================================

@interface Employee : Person

@property (nonatomic, strong) NSString *employeeId;
@property (nonatomic, strong) NSString *department;
@property (nonatomic, assign) CGFloat salary;

- (void)work;

@end

@implementation Employee

- (instancetype)initWithName:(NSString *)name
                  employeeId:(NSString *)employeeId
                  department:(NSString *)department {
    self = [super initWithName:name];
    if (self) {
        _employeeId = [employeeId copy];
        _department = [department copy];
    }
    return self;
}

- (void)sayHello {
    [super sayHello];
    NSLog(@"I work in %@", self.department);
}

- (void)work {
    NSLog(@"%@ is working", self.firstName);
}

@end

// ==============================================================================
// Blocks
// ==============================================================================

void demonstrateBlocks(void) {
    // Simple block
    void (^simpleBlock)(void) = ^{
        NSLog(@"Simple block executed");
    };
    simpleBlock();

    // Block with parameters
    int (^addBlock)(int, int) = ^int(int a, int b) {
        return a + b;
    };
    int sum = addBlock(5, 3);

    // Block capturing variables
    __block int counter = 0;
    void (^incrementBlock)(void) = ^{
        counter++;
    };

    // Block as parameter
    NSArray *numbers = @[@1, @2, @3, @4, @5];
    [numbers enumerateObjectsUsingBlock:^(id obj, NSUInteger idx, BOOL *stop) {
        NSLog(@"Index %lu: %@", (unsigned long)idx, obj);
        if (idx >= 3) *stop = YES;
    }];
}

// ==============================================================================
// GCD (Grand Central Dispatch)
// ==============================================================================

void demonstrateGCD(void) {
    // Main queue
    dispatch_async(dispatch_get_main_queue(), ^{
        // UI updates
    });

    // Background queue
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        // Background work
        dispatch_async(dispatch_get_main_queue(), ^{
            // Back to main thread
        });
    });

    // Custom queue
    dispatch_queue_t customQueue = dispatch_queue_create("com.example.myqueue", DISPATCH_QUEUE_SERIAL);
    dispatch_async(customQueue, ^{
        // Work on custom queue
    });

    // Dispatch after
    dispatch_after(dispatch_time(DISPATCH_TIME_NOW, (int64_t)(2.0 * NSEC_PER_SEC)),
                   dispatch_get_main_queue(), ^{
        NSLog(@"Executed after 2 seconds");
    });

    // Dispatch once
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        // Executed only once
    });

    // Dispatch group
    dispatch_group_t group = dispatch_group_create();
    dispatch_queue_t concurrentQueue = dispatch_queue_create("com.example.concurrent", DISPATCH_QUEUE_CONCURRENT);

    dispatch_group_async(group, concurrentQueue, ^{
        // Task 1
    });

    dispatch_group_async(group, concurrentQueue, ^{
        // Task 2
    });

    dispatch_group_notify(group, dispatch_get_main_queue(), ^{
        NSLog(@"All tasks completed");
    });

    // Dispatch semaphore
    dispatch_semaphore_t semaphore = dispatch_semaphore_create(1);
    dispatch_semaphore_wait(semaphore, DISPATCH_TIME_FOREVER);
    // Critical section
    dispatch_semaphore_signal(semaphore);
}

// ==============================================================================
// Foundation Classes Usage
// ==============================================================================

void demonstrateFoundation(void) {
    // NSString
    NSString *string = @"Hello, World!";
    NSString *formatted = [NSString stringWithFormat:@"Value: %d", 42];
    NSString *upper = [string uppercaseString];
    NSRange range = [string rangeOfString:@"World"];

    // NSMutableString
    NSMutableString *mutableString = [NSMutableString stringWithString:@"Hello"];
    [mutableString appendString:@" World"];

    // NSNumber
    NSNumber *intNumber = @42;
    NSNumber *floatNumber = @3.14f;
    NSNumber *boolNumber = @YES;
    int intValue = intNumber.intValue;

    // NSArray
    NSArray *array = @[@"one", @"two", @"three"];
    NSString *first = array.firstObject;
    NSArray *filtered = [array filteredArrayUsingPredicate:
        [NSPredicate predicateWithFormat:@"SELF BEGINSWITH 't'"]];

    // NSMutableArray
    NSMutableArray *mutableArray = [NSMutableArray arrayWithArray:array];
    [mutableArray addObject:@"four"];
    [mutableArray sortUsingSelector:@selector(compare:)];

    // NSDictionary
    NSDictionary *dict = @{@"key1": @"value1", @"key2": @"value2"};
    NSString *value = dict[@"key1"];

    // NSMutableDictionary
    NSMutableDictionary *mutableDict = [NSMutableDictionary dictionary];
    mutableDict[@"key"] = @"value";

    // NSDate
    NSDate *now = [NSDate date];
    NSDateFormatter *formatter = [[NSDateFormatter alloc] init];
    formatter.dateFormat = @"yyyy-MM-dd HH:mm:ss";
    NSString *dateString = [formatter stringFromDate:now];

    // NSURL
    NSURL *url = [NSURL URLWithString:@"https://example.com/path?query=value"];

    // NSError
    NSError *error = [NSError errorWithDomain:@"com.example" code:100 userInfo:@{
        NSLocalizedDescriptionKey: @"An error occurred"
    }];

    // NSNotificationCenter
    [[NSNotificationCenter defaultCenter] postNotificationName:@"MyNotification"
                                                        object:nil
                                                      userInfo:@{@"key": @"value"}];

    // NSUserDefaults
    NSUserDefaults *defaults = [NSUserDefaults standardUserDefaults];
    [defaults setObject:@"value" forKey:@"key"];
    [defaults synchronize];
}

// ==============================================================================
// Main Function
// ==============================================================================

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSLog(@"=== Objective-C Sample Program ===\n");

        // Create objects
        Person *person = [Person personWithName:@"John" age:30];
        person.lastName = @"Doe";
        person.email = @"john@example.com";
        [person sayHello];

        Employee *employee = [[Employee alloc] initWithName:@"Jane"
                                                 employeeId:@"EMP001"
                                                 department:@"Engineering"];
        employee.lastName = @"Smith";
        [employee sayHello];
        [employee work];

        // Try-catch
        @try {
            @throw [NSException exceptionWithName:@"TestException"
                                           reason:@"Testing exceptions"
                                         userInfo:nil];
        }
        @catch (NSException *exception) {
            NSLog(@"Caught exception: %@", exception.name);
        }
        @finally {
            NSLog(@"Finally block executed");
        }

        // Synchronized block
        id lockObject = [[NSObject alloc] init];
        @synchronized (lockObject) {
            // Thread-safe code
        }

        NSLog(@"\n=== Program Complete ===");
    }
    return 0;
}
