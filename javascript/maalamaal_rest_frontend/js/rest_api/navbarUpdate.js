let $signupButton = $('#signupButton');
let $loginButton = $('#loginButton');
let $logoutButton = $('#logoutButton');
let $profileButton = $('#profileButton');
let $welcomeUsername = $('#welcomeUsername');

let $navUsername = $('#navUsername');
let $navAccountType = $('#navAccountType ');

let token = Cookies.get('token');
let username = Cookies.get('username');
let account_type = Cookies.get('account_type');

if (token != null ) {
    console.log('User logged in as ' + username);
    $signupButton.hide();
    $loginButton.hide();
    $logoutButton.removeClass('d-none');

    $navUsername.text(username);
    $navAccountType.text(account_type);

    $welcomeUsername.text(username);
} else {
    $profileButton.hide();
    url = window.location.href;
    current_location = url.substring(url.lastIndexOf('/') + 1);
    console.log(current_location);
    console.log('User not logged in');
    if (current_location === 'profile.html') {
        console.log('Redirecting');
        window.location.href = '/';
    }

}
