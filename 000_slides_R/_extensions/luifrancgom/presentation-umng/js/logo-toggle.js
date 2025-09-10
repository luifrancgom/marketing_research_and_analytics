<script type="text/javascript">
  Reveal.on('ready', event => {
    if (event.indexh === 0) {
      document.querySelector("div.has-logo > img.slide-logo").style.display = "none";
      document.querySelector("div.footer-default").style.display = "none";
    }
  });

  Reveal.addEventListener('slidechanged', (event) => {
    if (event.indexh === 0) {
      document.querySelector("div.has-logo > img.slide-logo").style.display = "none";
      document.querySelector("div.footer-default").style.display = "none";
    } else {
      document.querySelector("div.has-logo > img.slide-logo").style.display = null;
      document.querySelector("div.footer-default").style.display = null;
    }
  });
</script>