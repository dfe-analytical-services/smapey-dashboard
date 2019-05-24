(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();
  a=s.createElement(o), m=s.getElementsByTagName(o)[0];
  a.async=1;
  a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

  ga('create', 'UA-140509958-1', 'auto');
  ga('send', 'pageview');


//Track which local authority is selected
  $(document).on('change', '#Local_Authority', function(e) {
    ga('send', 'event', 'widget', 'select Local Authority', $(e.currentTarget).val());
  });
