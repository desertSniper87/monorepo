## Sample Ecommerce site Backend

This is a example backend of a ecommerce site created using django rest framework. There is [frontend](https://github.com/desertSniper87/maalamaal_REST) made using html, jquery and mustache.

Web facing API is live at [heroku](http://maalamaal-rest-api.herokuapp.com/).

### How to run

#### Linux

It's a standard django site. 

1. Clone the repo. 
2. create virtualenv
    ```bash 
        python3 -m virtualenv env
    ```
3. Activate virtualenv 
    ```bash
       source env/bin/activate
    ```
4. If you use postgres then Create a database `maalamaal_rest`
 with the same username and password
 otherwise change database settings in `lab_test_rest/settings.py` 
 and then migrate by `./manage.py migrate`
5. Run server. `./manage.py runserver`

#### NixOS
I have included a `build.nix`. So running `build-nix` will create `@result` and you will be able to 
create virtualenv based on this
```bash
./result/bin/python3 -m virtualenv env
```

#### Todo (In order of Decreasing priority)

- [ ] Update/Delete Cart
- [ ] Update/Cancel Order
- [ ] Inline documentation
- [ ] Password validation on serverside.
- [ ] Order Notification in email.
- [ ] Stripe/Paypal integration.

```
ReadMe Last Updated: 2019-08-15, 20:03
```

#### License
one line to give the program's name and a brief description
Copyright © 2019 Samidhya Sarker

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the "Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

