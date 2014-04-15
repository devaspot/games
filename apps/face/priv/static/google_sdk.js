function plusoneCallback(auth){
  if(!auth['g-oauth-window']) {
    console.log('skip autologin');
  } else if(auth['access_token']){
    gapi.client.load('oauth2', 'v2', function(){
      gapi.client.oauth2.userinfo.get().execute(function(oauthResp){
        gapi.client.load('plus', 'v1', function(){
          gapi.client.plus.people.get({'userId':'me'}).execute(function(profileResp){
            if(plusLogin)plusLogin($.extend({}, gapi.auth.getToken(), oauthResp, profileResp, {'g-oauth-window': 'defined'})); }); }); }); });
  }else if(auth['error']){
        console.log('error');
  }
}

function render(){
  gapi.signin.render('plusloginbtn', {
    'callback':'plusoneCallback',
    'clientid':'146782506820.apps.googleusercontent.com',
    'cookiepolicy': 'http://synrc.com',
    'requestvisibleactions':'http://schemas.google.com/AddActivity',
    'scope':'https://www.googleapis.com/auth/plus.login https://www.googleapis.com/auth/userinfo.email '
  });
}

(function() {
  var po = document.createElement('script'); po.type = 'text/javascript'; po.async = true;
  po.src = 'https://apis.google.com/js/client:plusone.js?parsetags=explicit&onload=render';
  var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(po, s);
})();
