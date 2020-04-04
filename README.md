# Open Weather Map API

Provides services to connect to the
[Open Weather Map API](https://openweathermap.org/api).

Implemented services:

* Current weather for city id
* Current weather for geographical coordinates
* Current weather for multiple city ids

## Dependencies:

Requires

* GNATCOLL (for JSON parsing and logging)
* AWS (for web services)
* [SI_Units](https://github.com/HeisenbugLtd/si_units) (for formatting)
* An account at [openweathermap](https://openweathermap.org/home/sign_up)

## Quick start guide

1. Download, compile and include in your own project.
  * For now I am assuming you know how to do that (see gprinstall etc.). I may
    write more detailed instructions later.

2. Get your OpenWeatherMap API key
  1. Create an account at
     [openweathermap](https://openweathermap.org/home/sign_up).
  2. Log in to openweathermap.org
  3. In your account settings go to
     [API keys](https://home.openweathermap.org/api_keys).
  4. Create an API key.

3. Copy the config.json.example to ~/.config/openweathermap/config.json,
   preferrably with mode 0600.
  1. Fill in your API key information.
  2. If you have a direct connection to the internet, clear out the proxy
     configuration (empty strings), otherwise fill in the required information.
     
This completes the configuration step.

## API documentation

* Please note that this is still work in progress. For example, right now,
  there is no way of handling or even querying error information. While the
  software itself seems reasonably stable; if your internet connection drops or
  your API key is wrong an application using this API does have no way of
  knowing **what** went wrong. The only way to figure that out is to look at
  the log file (if you enabled the debug trace for that). Changes regarding
  this are planned in the near future.

If you just want to use the provided API without changes, you may start at the
source file `open_weather_map-api.ads` and use one of the `Create_XXX`
subroutines to create a query object. You may want to take a look at
`test/open_weather_map-application.adb` to see how the API is being used.

### Query objects

API queries are abstracted in `tagged types` specifically designed for a single
specific API query. All you need to do is to call `Perform_Query` and read out
the returned data. In case of errors etc. the `Current` parameter will be empty
(i.e. the discriminant `Valid` will be `False`).
