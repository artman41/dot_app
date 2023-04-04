# dot_app
Allows for additional data to be entered into the Application Resource file via an 'extra_data' prop.

```erlang
{application, dot_app, [
	{description, "New project"},
	{vsn, "0.1.0"},
  ...
	{extra_data, [hello_there]}
]}.
```

## How to use

- Append the `{extra_data, ...}` prop to your Application Resource.
- Load your application after `dot_app` has started.
- Call `dot_app:get_extra_data(Application)` to fetch your app's Extra Data.

## The how

This is achieved by pulling the AST for the `application` module via `code:get_object_code/1` and injecting calls to `dot_app_wrapper:handle_hook/3`.

On successful calls (calls to `application:load` that return an `ok`), 2 things happen:
1. Using the process' Stacktrace, we ensure that we're being called via the application module to prevent other calls from misusing the api
2. We consult the application's App Resource (`.app`) and try to look up the `extra_data` prop, defaulting to an empty list if nothing is found. This is then inserted as an additional element on the `#appl{}` record in the `ac_tab` table.
