window.onscroll = function () {
  if (document.body.scrollTop > 50 || document.documentElement.scrollTop > 50) {
    document.getElementById("toc").classList.add('fixed');
  } else {
    document.getElementById("toc").classList.remove('fixed');
  }
}
