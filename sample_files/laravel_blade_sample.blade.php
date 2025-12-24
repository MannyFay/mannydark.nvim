{{-- ==============================================================================
    Laravel Blade Sample - Syntax Highlighting Demonstration
    ============================================================================== --}}

{{-- This is a Blade comment - won't appear in HTML output --}}

<!DOCTYPE html>
<html lang="{{ str_replace('_', '-', app()->getLocale()) }}">
<head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <meta name="csrf-token" content="{{ csrf_token() }}">

    <title>{{ config('app.name', 'Laravel') }} - @yield('title', 'Home')</title>

    {{-- Vite Assets --}}
    @vite(['resources/css/app.css', 'resources/js/app.js'])

    {{-- Stack for page-specific styles --}}
    @stack('styles')
</head>
<body class="antialiased">
    {{-- ==========================================================================
        Layout Structure
        ========================================================================== --}}

    {{-- Include partial --}}
    @include('partials.header')

    {{-- Include with data --}}
    @include('partials.navigation', ['active' => 'home'])

    {{-- Include if exists --}}
    @includeIf('partials.sidebar')

    {{-- Include when condition --}}
    @includeWhen($user->isAdmin(), 'partials.admin-bar')

    {{-- Include unless condition --}}
    @includeUnless($user->isGuest(), 'partials.user-menu')

    {{-- Include first existing view --}}
    @includeFirst(['custom.header', 'default.header'])

    {{-- ==========================================================================
        Variables & Escaping
        ========================================================================== --}}

    {{-- Escaped output (XSS safe) --}}
    <h1>{{ $title }}</h1>
    <p>{{ $user->name }}</p>

    {{-- Unescaped output (raw HTML) - use with caution! --}}
    <div>{!! $htmlContent !!}</div>

    {{-- Default values with null coalescing --}}
    <p>{{ $name ?? 'Guest' }}</p>
    <p>{{ $user->nickname ?? $user->name ?? 'Anonymous' }}</p>

    {{-- Blade & JavaScript frameworks (preserve curly braces) --}}
    <div id="vue-app">
        @{{ message }}
        @{{ items.length }}
    </div>

    {{-- Verbatim - don't process as Blade --}}
    @verbatim
        <div id="react-app">
            {{ props.title }}
            {{ state.count }}
        </div>
    @endverbatim

    {{-- ==========================================================================
        Conditionals
        ========================================================================== --}}

    {{-- Basic if/else --}}
    @if($user->isAdmin())
        <span class="badge badge-admin">Administrator</span>
    @elseif($user->isModerator())
        <span class="badge badge-mod">Moderator</span>
    @else
        <span class="badge badge-user">User</span>
    @endif

    {{-- Unless (inverse of if) --}}
    @unless(Auth::check())
        <a href="/login">Please log in</a>
    @endunless

    {{-- Isset - check if variable exists and is not null --}}
    @isset($records)
        <p>Records count: {{ count($records) }}</p>
    @endisset

    {{-- Empty - check if variable is "empty" --}}
    @empty($notifications)
        <p>No notifications</p>
    @endempty

    {{-- Authentication directives --}}
    @auth
        <p>Welcome back, {{ Auth::user()->name }}!</p>
    @endauth

    @guest
        <p>Please sign in to continue</p>
    @endguest

    {{-- With specific guard --}}
    @auth('admin')
        <p>Admin area access granted</p>
    @endauth

    {{-- Environment checks --}}
    @env('local')
        <div class="debug-bar">Debug Mode Active</div>
    @endenv

    @env(['staging', 'production'])
        <!-- Analytics code here -->
    @endenv

    @production
        <script src="analytics.js"></script>
    @endproduction

    {{-- Has Section --}}
    @hasSection('sidebar')
        <div class="with-sidebar">
            @yield('sidebar')
        </div>
    @endif

    {{-- Section Missing --}}
    @sectionMissing('sidebar')
        <div class="full-width">
            @yield('content')
        </div>
    @endif

    {{-- ==========================================================================
        Switch Statement
        ========================================================================== --}}

    @switch($user->role)
        @case('admin')
            <p>Full access granted</p>
            @break

        @case('editor')
            <p>Edit access granted</p>
            @break

        @case('viewer')
            <p>Read-only access</p>
            @break

        @default
            <p>Limited access</p>
    @endswitch

    {{-- ==========================================================================
        Loops
        ========================================================================== --}}

    {{-- For loop --}}
    @for($i = 0; $i < 10; $i++)
        <p>Iteration {{ $i }}</p>
    @endfor

    {{-- Foreach loop --}}
    @foreach($users as $user)
        <li>{{ $user->name }} - {{ $user->email }}</li>
    @endforeach

    {{-- Foreach with $loop variable --}}
    @foreach($items as $item)
        @if($loop->first)
            <p>First item!</p>
        @endif

        <li class="{{ $loop->even ? 'even' : 'odd' }}">
            {{ $loop->iteration }}/{{ $loop->count }}: {{ $item->name }}
            @if($loop->last)
                (Last item)
            @endif
        </li>

        {{-- Nested loop --}}
        @foreach($item->children as $child)
            <span>Parent iteration: {{ $loop->parent->iteration }}</span>
            <span>Child: {{ $child->name }}</span>
        @endforeach
    @endforeach

    {{-- Forelse - foreach with empty fallback --}}
    @forelse($posts as $post)
        <article>
            <h2>{{ $post->title }}</h2>
            <p>{{ $post->excerpt }}</p>
        </article>
    @empty
        <p>No posts available.</p>
    @endforelse

    {{-- While loop --}}
    @while($condition)
        <p>Still true...</p>
    @endwhile

    {{-- Loop control --}}
    @foreach($users as $user)
        @if($user->isBanned())
            @continue
        @endif

        @if($user->id === $targetId)
            @break
        @endif

        <p>{{ $user->name }}</p>
    @endforeach

    {{-- Continue/break with conditions --}}
    @foreach($users as $user)
        @continue($user->isBanned())
        @break($user->id === 100)
        <p>{{ $user->name }}</p>
    @endforeach

    {{-- ==========================================================================
        Components
        ========================================================================== --}}

    {{-- Anonymous component --}}
    <x-alert type="warning" message="Please verify your email" />

    {{-- Component with slot --}}
    <x-card title="User Profile">
        <p>This is the card content</p>
    </x-card>

    {{-- Component with named slots --}}
    <x-modal>
        <x-slot:title>
            Confirm Action
        </x-slot>

        <x-slot name="footer">
            <button>Cancel</button>
            <button>Confirm</button>
        </x-slot>

        <p>Are you sure you want to proceed?</p>
    </x-modal>

    {{-- Component with attributes --}}
    <x-button
        type="submit"
        class="btn-primary"
        :disabled="$isProcessing"
        wire:click="save"
    >
        Save Changes
    </x-button>

    {{-- Dynamic component --}}
    <x-dynamic-component :component="$componentName" :attributes="$attributes" />

    {{-- Component with scoped slots --}}
    <x-data-table :items="$users">
        <x-slot:header>
            <th>Name</th>
            <th>Email</th>
        </x-slot>

        <x-slot:row :item="$user">
            <td>{{ $user->name }}</td>
            <td>{{ $user->email }}</td>
        </x-slot>
    </x-data-table>

    {{-- ==========================================================================
        Forms
        ========================================================================== --}}

    <form method="POST" action="{{ route('users.store') }}">
        {{-- CSRF Protection --}}
        @csrf

        {{-- Method spoofing for PUT/PATCH/DELETE --}}
        @method('PUT')

        {{-- Old input value --}}
        <input type="text" name="name" value="{{ old('name', $user->name ?? '') }}">

        {{-- Error handling --}}
        @error('name')
            <span class="error">{{ $message }}</span>
        @enderror

        {{-- Error with specific bag --}}
        @error('email', 'login')
            <span class="error">{{ $message }}</span>
        @enderror

        {{-- Check if errors exist --}}
        @if($errors->any())
            <ul class="errors">
                @foreach($errors->all() as $error)
                    <li>{{ $error }}</li>
                @endforeach
            </ul>
        @endif

        <button type="submit">Submit</button>
    </form>

    {{-- ==========================================================================
        Authorization
        ========================================================================== --}}

    @can('update', $post)
        <a href="{{ route('posts.edit', $post) }}">Edit Post</a>
    @endcan

    @cannot('delete', $post)
        <p>You cannot delete this post</p>
    @endcannot

    @canany(['update', 'delete'], $post)
        <div class="admin-actions">
            <!-- Show admin actions -->
        </div>
    @endcanany

    @canany(['create', 'update'], App\Models\Post::class)
        <a href="{{ route('posts.create') }}">New Post</a>
    @elsecanany(['view'], App\Models\Post::class)
        <p>You can only view posts</p>
    @endcanany

    {{-- ==========================================================================
        Sections & Layouts
        ========================================================================== --}}

    {{-- Extend a layout --}}
    @extends('layouts.app')

    {{-- Define a section --}}
    @section('content')
        <div class="container">
            <h1>Page Content</h1>
        </div>
    @endsection

    {{-- Section with default content --}}
    @section('sidebar')
        @parent {{-- Include parent section content --}}
        <p>Additional sidebar content</p>
    @endsection

    {{-- Yield a section --}}
    @yield('content')

    {{-- Yield with default --}}
    @yield('sidebar', 'Default sidebar content')

    {{-- Show section (alternative to yield) --}}
    @section('footer')
        <footer>Copyright 2024</footer>
    @show

    {{-- ==========================================================================
        Stacks (for CSS/JS)
        ========================================================================== --}}

    {{-- Push to stack --}}
    @push('scripts')
        <script src="/js/custom.js"></script>
    @endpush

    {{-- Push once (prevents duplicates) --}}
    @pushOnce('scripts')
        <script src="/js/library.js"></script>
    @endPushOnce

    {{-- Prepend to stack --}}
    @prepend('scripts')
        <script src="/js/priority.js"></script>
    @endprepend

    {{-- Output stack --}}
    @stack('scripts')

    {{-- ==========================================================================
        Raw PHP
        ========================================================================== --}}

    @php
        $counter = 0;
        $items = collect($data)->filter(fn($item) => $item->active);

        foreach ($items as $item) {
            $counter++;
        }
    @endphp

    {{-- Single line PHP --}}
    @php($activeUsers = $users->where('active', true))

    {{-- ==========================================================================
        JavaScript & JSON
        ========================================================================== --}}

    <script>
        // Pass PHP data to JavaScript
        const config = @json($config);
        const users = @json($users, JSON_PRETTY_PRINT);

        // Blade in JavaScript (escaped)
        const appName = "{{ config('app.name') }}";
        const userId = {{ Auth::id() ?? 'null' }};

        // With Js facade (Laravel 9+)
        const settings = {{ Js::from($settings) }};
    </script>

    {{-- ==========================================================================
        Custom Directives
        ========================================================================== --}}

    {{-- Custom datetime directive --}}
    @datetime($user->created_at)

    {{-- Custom money directive --}}
    @money($product->price)

    {{-- Custom directive with multiple arguments --}}
    @truncate($text, 100, '...')

    {{-- ==========================================================================
        Class & Style Directives
        ========================================================================== --}}

    {{-- Conditional classes --}}
    <div @class([
        'p-4',
        'bg-red-500' => $hasError,
        'bg-green-500' => !$hasError,
        'font-bold' => $isImportant,
    ])>
        Content with conditional classes
    </div>

    {{-- Conditional styles --}}
    <div @style([
        'background-color: red' => $hasError,
        'font-weight: bold',
        'color: ' . $textColor,
    ])>
        Content with conditional styles
    </div>

    {{-- Checked, selected, disabled, readonly, required --}}
    <input type="checkbox" @checked($user->active)>
    <option @selected($option === $current)>{{ $option }}</option>
    <button @disabled($isProcessing)>Submit</button>
    <input type="text" @readonly($user->isLocked())>
    <input type="email" @required($isRequired)>

    {{-- ==========================================================================
        Livewire Integration
        ========================================================================== --}}

    {{-- Livewire component --}}
    @livewire('search-users', ['role' => 'admin'])

    {{-- Livewire with key --}}
    @livewire('user-card', ['user' => $user], key($user->id))

    {{-- Livewire styles and scripts --}}
    @livewireStyles
    @livewireScripts

    {{-- ==========================================================================
        Service Injection
        ========================================================================== --}}

    @inject('metrics', 'App\Services\MetricsService')

    <div>
        Monthly Revenue: {{ $metrics->monthlyRevenue() }}
    </div>

    {{-- ==========================================================================
        Fragments (Laravel 9+)
        ========================================================================== --}}

    @fragment('user-list')
        <ul>
            @foreach($users as $user)
                <li>{{ $user->name }}</li>
            @endforeach
        </ul>
    @endfragment

    {{-- ==========================================================================
        Once (render only once)
        ========================================================================== --}}

    @once
        <style>
            .special-component { color: blue; }
        </style>
    @endonce

    {{-- ==========================================================================
        Session
        ========================================================================== --}}

    @session('status')
        <div class="alert alert-success">
            {{ $value }}
        </div>
    @endsession

    {{-- Include footer --}}
    @include('partials.footer')

</body>
</html>
