%% ==============================================================================
%% Comprehensive MATLAB Sample - Syntax Highlighting Demonstration
%% ==============================================================================

% This file demonstrates all major MATLAB language features
% for syntax highlighting purposes.

%% ==============================================================================
%% Comments
%% ==============================================================================

% Single line comment
%% Cell title (section marker)

%{
Multi-line
block comment
%}

%% ==============================================================================
%% Variables and Data Types
%% ==============================================================================

% Numeric variables
integerVal = 42;
doubleVal = 3.14159;
scientificVal = 6.022e23;
negativeScientific = 1.0e-10;
complexVal = 3 + 4i;
complexAlt = 3 + 4j;
complexPure = 1i;
hexVal = 0xDEADBEEF;
binaryVal = 0b11010110;

% Special values
infVal = Inf;
negInfVal = -Inf;
nanVal = NaN;
piVal = pi;
eulerVal = exp(1);
epsVal = eps;
realmaxVal = realmax;
realminVal = realmin;

% Logical values
trueVal = true;
falseVal = false;
logicalArray = [true, false, true];

% Character arrays and strings
charArray = 'Hello, MATLAB!';
stringVal = "Hello, MATLAB!";  % String (R2016b+)
escapedChar = 'It''s working';  % Escape single quote
multilineStr = ['Line 1', newline, 'Line 2'];

% String operations
str1 = "Hello";
str2 = "World";
concatenated = str1 + " " + str2;
formatted = sprintf("Value: %d, Float: %.2f", 42, 3.14);
converted = num2str(42);
converted2 = str2num('42');

% Empty arrays
emptyDouble = [];
emptyCell = {};
emptyStruct = struct([]);

% NaN and empty checking
isnan(NaN);
isempty([]);
isinf(Inf);
isfinite(42);

%% ==============================================================================
%% Vectors and Matrices
%% ==============================================================================

% Row vector
rowVec = [1, 2, 3, 4, 5];
rowVecAlt = [1 2 3 4 5];

% Column vector
colVec = [1; 2; 3; 4; 5];
colVecTranspose = [1, 2, 3, 4, 5]';

% Matrix
mat = [1 2 3; 4 5 6; 7 8 9];
mat2 = [1, 2, 3; 4, 5, 6; 7, 8, 9];

% Colon operator
range = 1:10;
rangeStep = 1:2:10;
rangeFloat = 0:0.1:1;
reverseRange = 10:-1:1;

% Special matrices
zerosMatrix = zeros(3, 4);
onesMatrix = ones(3, 4);
eyeMatrix = eye(3);
randMatrix = rand(3, 4);
randnMatrix = randn(3, 4);
diagMatrix = diag([1, 2, 3]);
meshMatrix = meshgrid(1:3, 1:4);
[X, Y] = meshgrid(-2:0.1:2, -2:0.1:2);
linspaceVec = linspace(0, 1, 11);
logspaceVec = logspace(0, 2, 11);

% Matrix indexing (1-based!)
element = mat(2, 3);       % Element at row 2, col 3
row = mat(1, :);           % First row
col = mat(:, 2);           % Second column
subMat = mat(1:2, 2:3);    % Submatrix
lastElement = mat(end, end);
linear = mat(5);           % Linear indexing
logical = mat(mat > 5);    % Logical indexing

% Matrix operations
transposed = mat';         % Conjugate transpose
transposeNoConj = mat.';   % Transpose without conjugate
matMult = mat * mat;       % Matrix multiplication
elemMult = mat .* mat;     % Element-wise multiplication
elemDiv = mat ./ mat;      % Element-wise division
elemPow = mat .^ 2;        % Element-wise power
leftDiv = mat \ colVec;    % Left division (solve Ax = b)
rightDiv = rowVec / mat;   % Right division

% Matrix concatenation
horizontal = [mat, mat];
vertical = [mat; mat];
catDim1 = cat(1, mat, mat);
catDim2 = cat(2, mat, mat);

% Matrix manipulation
reshaped = reshape(1:12, 3, 4);
flipped = flip(mat);
flippedLR = fliplr(mat);
flippedUD = flipud(mat);
rotated = rot90(mat);
squeezed = squeeze(ones(3, 1, 4));
permuted = permute(mat, [2, 1]);
repmatted = repmat(mat, 2, 3);

% Matrix properties
[rows, cols] = size(mat);
numElements = numel(mat);
numDims = ndims(mat);
len = length(rowVec);

%% ==============================================================================
%% Cell Arrays
%% ==============================================================================

% Create cell arrays
cellArr = {1, 'hello', [1 2 3], true};
cellMat = {1, 2; 3, 4};
emptyCell = cell(3, 4);

% Cell indexing
cellContent = cellArr{1};      % Content access
cellElement = cellArr(1);      % Cell access (returns cell)
multiContent = cellArr{1:2};   % Multiple contents

% Cell operations
converted = cell2mat({1, 2; 3, 4});
converted2 = mat2cell(mat, [1 2], [2 1]);
numCell = num2cell(1:5);
strCell = cellstr(['abc'; 'def']);

%% ==============================================================================
%% Structures
%% ==============================================================================

% Create structure
person.name = 'Alice';
person.age = 30;
person.city = 'Seattle';

% Structure with struct function
person2 = struct('name', 'Bob', 'age', 25, 'city', 'Portland');

% Structure array
people(1).name = 'Alice';
people(1).age = 30;
people(2).name = 'Bob';
people(2).age = 25;

% Alternative creation
names = {'Alice', 'Bob', 'Charlie'};
ages = {30, 25, 35};
peopleArr = struct('name', names, 'age', ages);

% Access fields
nameVal = person.name;
ageVal = person.('age');  % Dynamic field access
fieldName = 'city';
cityVal = person.(fieldName);

% Structure operations
fields = fieldnames(person);
hasField = isfield(person, 'name');
removed = rmfield(person, 'city');
ordered = orderfields(person);

% Nested structures
company.name = 'TechCorp';
company.ceo.name = 'Alice';
company.ceo.age = 45;
company.employees = people;

%% ==============================================================================
%% Tables (R2013b+)
%% ==============================================================================

% Create table
Name = {'Alice'; 'Bob'; 'Charlie'};
Age = [30; 25; 35];
Salary = [75000; 60000; 80000];
T = table(Name, Age, Salary);

% Table from arrays
T2 = array2table([1 2; 3 4], 'VariableNames', {'A', 'B'});

% Access table data
T.Name;               % Column access
T(1, :);              % Row access
T{1, 'Name'};         % Element access
T.Properties.VariableNames;

% Table operations
sortedT = sortrows(T, 'Age');
filteredT = T(T.Age > 25, :);
joinedT = join(T, T2);

%% ==============================================================================
%% Control Flow
%% ==============================================================================

% If-else
x = 10;
if x > 0
    disp('Positive');
elseif x < 0
    disp('Negative');
else
    disp('Zero');
end

% Switch-case
day = 'Monday';
switch day
    case 'Monday'
        disp('Start of week');
    case {'Saturday', 'Sunday'}
        disp('Weekend');
    otherwise
        disp('Weekday');
end

% For loop
for i = 1:10
    disp(i);
end

for i = [1, 3, 5, 7, 9]
    disp(i);
end

for i = mat
    disp(i);
end

% While loop
count = 0;
while count < 5
    disp(count);
    count = count + 1;
end

% Loop control
for i = 1:10
    if i == 3
        continue;
    end
    if i == 8
        break;
    end
    disp(i);
end

% Vectorized operations (preferred over loops)
result = arrayfun(@(x) x^2, 1:10);
result2 = cellfun(@length, {'a', 'bb', 'ccc'});
result3 = structfun(@(x) x * 2, struct('a', 1, 'b', 2));

%% ==============================================================================
%% Functions
%% ==============================================================================

% Function in script (local function, must be at end)
% See end of file for function definitions

% Anonymous functions
square = @(x) x.^2;
add = @(x, y) x + y;
multiline = @(x) ...
    x.^2 + ...
    2*x + 1;

% Function handles
fhandle = @sin;
result = fhandle(pi/2);

% Apply functions
result = feval(@sin, pi/2);
result2 = cellfun(@(x) x^2, {1, 2, 3});
result3 = arrayfun(@(x) x^2, 1:5);

% Function composition
f = @(x) x.^2;
g = @(x) sin(x);
composed = @(x) f(g(x));

%% ==============================================================================
%% Classes and OOP
%% ==============================================================================

% Class definition (typically in separate file)
% classdef MyClass < handle
%     properties
%         Value
%     end
%     properties (Access = private)
%         PrivateValue
%     end
%     properties (Constant)
%         PI = 3.14159
%     end
%     methods
%         function obj = MyClass(val)
%             obj.Value = val;
%         end
%         function display(obj)
%             disp(obj.Value);
%         end
%     end
%     methods (Static)
%         function result = staticMethod()
%             result = 'static';
%         end
%     end
%     methods (Access = protected)
%         function protectedMethod(obj)
%         end
%     end
%     events
%         ValueChanged
%     end
% end

% Enumeration class
% classdef Color
%     enumeration
%         Red, Green, Blue
%     end
% end

%% ==============================================================================
%% Error Handling
%% ==============================================================================

% Try-catch
try
    result = 1 / 0;
catch ME
    disp(['Error: ', ME.message]);
    disp(['Identifier: ', ME.identifier]);
end

% Try-catch with rethrow
try
    error('MYERROR:test', 'Test error message');
catch ME
    if strcmp(ME.identifier, 'MYERROR:test')
        disp('Caught expected error');
    else
        rethrow(ME);
    end
end

% Assert
assert(x > 0, 'x must be positive');
assert(x > 0, 'MYERROR:test', 'x must be positive');

% Error and warning
% error('Error message');
% error('MYERROR:id', 'Error with %s', 'formatting');
warning('Warning message');
warning('off', 'all');
warning('on', 'all');

% Last error/warning
lastError = lasterror;
lastWarning = lastwarn;

%% ==============================================================================
%% File I/O
%% ==============================================================================

% Text files
% fid = fopen('file.txt', 'r');
% content = fread(fid, '*char')';
% fclose(fid);

% fid = fopen('file.txt', 'w');
% fprintf(fid, 'Hello, %s!\n', 'MATLAB');
% fclose(fid);

% Read entire file
% content = fileread('file.txt');

% Read lines
% lines = readlines('file.txt');

% CSV files
% data = csvread('data.csv');
% data = readtable('data.csv');
% csvwrite('output.csv', data);
% writetable(T, 'output.csv');

% MAT files
% save('data.mat', 'x', 'y', 'z');
% load('data.mat');
% save('data.mat', '-v7.3');  % Large files

% Excel files
% data = xlsread('file.xlsx');
% [num, txt, raw] = xlsread('file.xlsx', 'Sheet1');
% xlswrite('output.xlsx', data);

% Image files
% img = imread('image.png');
% imwrite(img, 'output.png');

% Audio files
% [y, fs] = audioread('audio.wav');
% audiowrite('output.wav', y, fs);

% File operations
% exist('file.txt', 'file');
% delete('file.txt');
% copyfile('src.txt', 'dst.txt');
% movefile('old.txt', 'new.txt');
% mkdir('newdir');
% rmdir('dir');
% cd('path');
% pwd;
% dir;
% ls;

%% ==============================================================================
%% Plotting
%% ==============================================================================

% 2D Plots
x = linspace(0, 2*pi, 100);
y = sin(x);

figure;
plot(x, y);
plot(x, y, 'r--', 'LineWidth', 2);
plot(x, y, 'bo-', 'MarkerSize', 5);

% Multiple plots
hold on;
plot(x, cos(x), 'g-.');
hold off;

% Subplots
subplot(2, 2, 1);
plot(x, sin(x));
subplot(2, 2, 2);
plot(x, cos(x));
subplot(2, 2, 3);
plot(x, tan(x));
subplot(2, 2, 4);
plot(x, exp(-x));

% Plot customization
title('My Plot');
xlabel('X Axis');
ylabel('Y Axis');
legend('sin', 'cos');
grid on;
axis([0 2*pi -1 1]);
xlim([0 2*pi]);
ylim([-1 1]);
set(gca, 'FontSize', 12);

% Other 2D plots
bar([1 2 3 4]);
barh([1 2 3 4]);
histogram(randn(1000, 1));
pie([1 2 3 4]);
scatter(rand(20, 1), rand(20, 1));
stem(1:10);
stairs(1:10);
errorbar(1:5, rand(1, 5), 0.1*ones(1, 5));
area(rand(10, 3));
polarplot(0:0.1:2*pi, sin(0:0.1:2*pi));

% 3D Plots
[X, Y] = meshgrid(-2:0.1:2, -2:0.1:2);
Z = X.^2 + Y.^2;

figure;
surf(X, Y, Z);
mesh(X, Y, Z);
contour(X, Y, Z);
contourf(X, Y, Z);
plot3(sin(0:0.1:10), cos(0:0.1:10), 0:0.1:10);
scatter3(rand(20, 1), rand(20, 1), rand(20, 1));

% Colormap
colormap('jet');
colormap('hot');
colormap('parula');
colorbar;

% Figure operations
figure('Name', 'My Figure', 'NumberTitle', 'off');
clf;
close;
close all;

% Save figure
% saveas(gcf, 'figure.png');
% print('-dpng', 'figure.png');
% exportgraphics(gcf, 'figure.pdf');

%% ==============================================================================
%% Linear Algebra
%% ==============================================================================

A = [1 2; 3 4];
b = [5; 6];

% Basic operations
detA = det(A);
invA = inv(A);
traceA = trace(A);
rankA = rank(A);
nullA = null(A);
orthA = orth(A);

% Solve linear system Ax = b
x = A \ b;
x2 = linsolve(A, b);

% Eigenvalues and eigenvectors
eigenvalues = eig(A);
[V, D] = eig(A);

% Singular value decomposition
[U, S, V] = svd(A);

% Matrix decompositions
[L, U] = lu(A);
[L, U, P] = lu(A);
[Q, R] = qr(A);
R = chol(A' * A);

% Norms
normA = norm(A);          % 2-norm
normA1 = norm(A, 1);      % 1-norm
normAInf = norm(A, inf);  % Inf-norm
normAFro = norm(A, 'fro'); % Frobenius norm

% Condition number
condA = cond(A);

%% ==============================================================================
%% Statistics
%% ==============================================================================

data = randn(100, 1);

% Descriptive statistics
meanVal = mean(data);
medianVal = median(data);
modeVal = mode(data);
stdVal = std(data);
varVal = var(data);
minVal = min(data);
maxVal = max(data);
rangeVal = range(data);
sumVal = sum(data);
prodVal = prod(data);

% Percentiles and quartiles
prctile(data, [25 50 75]);
quantile(data, [0.25 0.5 0.75]);
iqr(data);

% Correlation and covariance
corrcoef(data, data);
cov(data, data);

% Cumulative operations
cumsum(data);
cumprod(data);
cummin(data);
cummax(data);

% Distributions
normrnd(0, 1, 100, 1);          % Normal random
unifrnd(0, 1, 100, 1);          % Uniform random
exprnd(1, 100, 1);              % Exponential random
normpdf(0);                     % Normal PDF
normcdf(0);                     % Normal CDF
norminv(0.5);                   % Normal inverse CDF

% Statistical tests
[h, p] = ttest(data);
[h, p] = ttest2(data, randn(100, 1));
[h, p] = chi2gof(data);
[h, p] = kstest(data);

% Regression
X = [ones(100, 1), randn(100, 1)];
y = randn(100, 1);
beta = X \ y;
% mdl = fitlm(X, y);

%% ==============================================================================
%% Signal Processing
%% ==============================================================================

% Generate signals
t = 0:0.001:1;
signal = sin(2*pi*10*t) + 0.5*sin(2*pi*20*t);

% FFT
Y = fft(signal);
P2 = abs(Y/length(signal));
P1 = P2(1:length(signal)/2+1);

% Filtering
% [b, a] = butter(4, 0.5);  % Butterworth filter
% filtered = filter(b, a, signal);
% filtered = filtfilt(b, a, signal);

% Convolution
conv([1 2 3], [4 5 6]);
conv([1 2 3], [4 5 6], 'same');

% Cross-correlation
xcorr(signal, signal);

% Window functions
hamming(256);
hanning(256);
blackman(256);
kaiser(256, 5);

%% ==============================================================================
%% Image Processing
%% ==============================================================================

% Create sample image
img = rand(100, 100);

% Image display
% imshow(img);
% imagesc(img);
% imshow(img, [0 1]);

% Image operations
imgResize = imresize(img, 0.5);
imgRotate = imrotate(img, 45);
imgCrop = imcrop(img, [10 10 50 50]);
imgFlip = flipud(img);

% Color conversion
% grayImg = rgb2gray(colorImg);
% hsvImg = rgb2hsv(colorImg);

% Filtering
% imgBlur = imgaussfilt(img, 2);
% imgEdge = edge(img, 'canny');
% imgMed = medfilt2(img);

% Morphological operations
se = strel('disk', 5);
% imgDilate = imdilate(img > 0.5, se);
% imgErode = imerode(img > 0.5, se);
% imgOpen = imopen(img > 0.5, se);
% imgClose = imclose(img > 0.5, se);

%% ==============================================================================
%% Optimization
%% ==============================================================================

% Function minimization
f = @(x) x^2 - 4*x + 4;
% x = fminbnd(f, 0, 5);
% x = fminsearch(f, 0);

% Multivariable optimization
f2 = @(x) x(1)^2 + x(2)^2;
% x = fminsearch(f2, [1, 1]);
% x = fminunc(f2, [1, 1]);

% Constrained optimization
% x = fmincon(f2, [1, 1], [], [], [], [], [-1, -1], [1, 1]);

% Linear programming
% [x, fval] = linprog(f, A, b);

% Root finding
f3 = @(x) x^2 - 2;
% x = fzero(f3, 1);

% Solving equations
% x = fsolve(@(x) [x(1)^2 + x(2) - 1; x(1) + x(2)^2 - 1], [0.5, 0.5]);

%% ==============================================================================
%% Numerical Methods
%% ==============================================================================

% Integration
f = @(x) x.^2;
% result = integral(f, 0, 1);
% result = integral2(@(x, y) x.^2 + y.^2, 0, 1, 0, 1);

% Differentiation (numerical)
% dydx = diff(y) ./ diff(x);
% dydx = gradient(y, x);

% ODE solving
% [t, y] = ode45(@(t, y) -y, [0 10], 1);
% [t, y] = ode23(@(t, y) -y, [0 10], 1);
% [t, y] = ode15s(@(t, y) -y, [0 10], 1);

% Interpolation
xi = 0:0.1:10;
yi = interp1(x, y, xi);
yi = interp1(x, y, xi, 'spline');
yi = interp1(x, y, xi, 'pchip');

% Curve fitting
% p = polyfit(x, y, 2);  % Polynomial fit
% yfit = polyval(p, x);

% Splines
% pp = spline(x, y);
% yi = ppval(pp, xi);

%% ==============================================================================
%% Parallel Computing
%% ==============================================================================

% Check parallel pool
% gcp;

% Create parallel pool
% parpool(4);

% Parallel for loop
% parfor i = 1:100
%     result(i) = heavyComputation(i);
% end

% SPMD (Single Program Multiple Data)
% spmd
%     labindex;  % Worker ID
%     numlabs;   % Number of workers
% end

% GPU computing
% gpuDevice;
% g = gpuArray(rand(1000));
% result = gather(g * g);

% Distributed arrays
% d = distributed(rand(1000));

%% ==============================================================================
%% Symbolic Math
%% ==============================================================================

% Symbolic variables
% syms x y z
% syms f(x)

% Symbolic expressions
% expr = x^2 + 2*x + 1;
% expanded = expand((x + 1)^2);
% factored = factor(x^2 - 1);
% simplified = simplify(sin(x)^2 + cos(x)^2);

% Calculus
% diff(x^2, x);       % Differentiation
% int(x^2, x);        % Integration
% limit(sin(x)/x, x, 0);  % Limit

% Solve equations
% solve(x^2 - 1 == 0, x);
% dsolve(diff(y, t) == -y);  % ODE

% Substitution
% subs(x^2, x, 2);

% Convert to function
% f = matlabFunction(x^2);

%% ==============================================================================
%% Simulink (references only)
%% ==============================================================================

% Open Simulink
% simulink;

% Open model
% open_system('model_name');

% Run simulation
% sim('model_name');

% Get/set parameters
% get_param('model_name/block', 'parameter');
% set_param('model_name/block', 'parameter', 'value');

%% ==============================================================================
%% MEX and External Interfaces
%% ==============================================================================

% Compile MEX file
% mex myfunction.c

% Call MEX function
% result = myfunction(args);

% Python interface
% py.numpy.array([1, 2, 3]);
% py.list({1, 2, 3});

% Java interface
% javaObject('java.util.ArrayList');

% Call external program
% system('command');
% [status, result] = system('command');
% ! command

%% ==============================================================================
%% Debugging
%% ==============================================================================

% Display and print
disp('Message');
disp(variable);
fprintf('Value: %d\n', 42);

% Debug commands
% dbstop in function
% dbstop at line
% dbstop if error
% dbclear all
% dbcont
% dbstep
% dbquit

% Profiling
% profile on;
% profile off;
% profile viewer;

% Timing
tic;
% code to time
elapsed = toc;

timeit(@() sin(1:1000000));

%% ==============================================================================
%% Package and Namespace
%% ==============================================================================

% Call function from package
% result = mypackage.myfunction(args);

% Import
% import mypackage.*
% import mypackage.myfunction

%% ==============================================================================
%% Local Functions (must be at end of script/function file)
%% ==============================================================================

function result = add(a, b)
    % Add two numbers
    %
    % Inputs:
    %   a - First number
    %   b - Second number
    %
    % Outputs:
    %   result - Sum of a and b

    result = a + b;
end

function [quotient, remainder] = divide(a, b)
    % Divide a by b and return quotient and remainder

    arguments
        a (1,1) double
        b (1,1) double {mustBeNonzero}
    end

    quotient = floor(a / b);
    remainder = mod(a, b);
end

function result = variableArgs(varargin)
    % Function with variable arguments

    result = 0;
    for i = 1:nargin
        result = result + varargin{i};
    end
end

function varargout = multipleOutputs(n)
    % Function with variable number of outputs

    for i = 1:nargout
        varargout{i} = i * n;
    end
end

function result = withValidation(x, options)
    % Function with argument validation (R2019b+)

    arguments
        x (:,:) double {mustBeNumeric, mustBeReal}
        options.Scale (1,1) double = 1
        options.Offset (1,1) double = 0
    end

    result = options.Scale * x + options.Offset;
end
