function plusoneCallback(auth){
  if (!auth['g-oauth-window']) { console.log('skip autologin'); }
  else if(auth['access_token']) {
    gapi.client.load('oauth2', 'v2', function(){
      gapi.client.oauth2.userinfo.get().execute(function(oauthResp){
            if(plusLogin) plusLogin(oauthResp);
     }); });
  } else if(auth['error']) { console.log('error'); }
}

function render(){
  gapi.signin.render('plusloginbtn', {
    'callback':'plusoneCallback',
    'clientid':'146782506820.apps.googleusercontent.com',
    'cookiepolicy': 'http://skyline.synrc.com',
    'requestvisibleactions':'http://schemas.google.com/AddActivity',
    'scope':'https://www.googleapis.com/auth/plus.login https://www.googleapis.com/auth/userinfo.email '
  });
}

(function() {
  var po = document.createElement('script'); po.type = 'text/javascript'; po.async = true;
  po.src = 'https://apis.google.com/js/client:plusone.js?parsetags=explicit&onload=render';
  var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(po, s);
})();
