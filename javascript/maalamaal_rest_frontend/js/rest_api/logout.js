$logoutButton = $('#logoutButton');

function logout() {
    Cookies.remove('username');
    Cookies.remove('token');
    Cookies.remove('account_type');
    window.location.reload();
}

$(document).ready(function(){
    $logoutButton.click(logout);
});


