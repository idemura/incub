function parseJSON(res) {
  return JSON && JSON.parse(res) || $.parseJSON(res)
}

function watchAuth(email) {
  function login(assertion) {
    $.ajax({
      url: '/login',
      type: 'POST',
      data: {assertion: assertion},
      success: function(res, status, xhr) {
        var obj = parseJSON(res);
        switch (obj.Status) {
        default:
        case 0:
          window.location.reload();
          break;
        case 1:
          window.location = '/newuserform';
          break;
        }
      },
    });
  }
  function logout() {
    $.ajax({
      url: '/logout', // This is a URL on your website.
      type: 'POST',
      success: function(res, status, xhr) {
        window.location.reload();
      }
    });
  }

  navigator.id.watch({
    loggedInUser: email,
    onlogin: login,
    onlogout: logout,
  });
}
