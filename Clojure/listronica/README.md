# Listronica

This is the example app that we develop in the [Web
Development in
Clojure](https://purelyfunctional.tv/courses/web-dev-in-clojure/)
course at
[PurelyFunctional.tv](https://purelyfunctional.tv/).

There are branches for each exercise in the course. The
final server with all code from the course is on the
`master` branch.

## Usage

### Database setup

Make sure you have PostgreSQL installed and running. You
will need a database called "webdev".

See the [PostgreSQL Install
Guide](http://postgresguide.com/setup/install.html) for
instructions for multiple operating systems.

Create the database.

```bash
$CMD createdb webdev
```

### The Clojure server

Run the server:

```bash
$CMD lein run 8989
```

That will run the server on port 8989. You can change the
port to whatever works on your system.

Visit these urls to see it in action:

* http://localhost:8989/
* http://localhost:8989/goodbye
* http://localhost:8989/yo/Eric
* http://localhost:8989/calc/7/+/900
* http://localhost:8989/about
* http://localhost:8989/request

And the big todo list app:

* http://localhost:8989/items

## License

[![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)](http://creativecommons.org/publicdomain/zero/1.0/)

To the extent possible under law, the person who associated CC0 with
this work has waived all copyright and related or neighboring rights
to the code in this repository.

See the `LICENSE` file for more information.

