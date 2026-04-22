// Système de traduction FR/EN partagé entre toutes les pages
(function() {
  function getLang() { return localStorage.getItem('lang') || 'fr'; }
  function setLang(l) { localStorage.setItem('lang', l); }

  function applyLang(lang) {
    document.querySelectorAll('[data-fr]').forEach(function(el) {
      el.innerHTML = lang === 'en' ? el.getAttribute('data-en') : el.getAttribute('data-fr');
    });

    // Titre onglet
    var titleEl = document.querySelector('title[data-title-fr]');
    if (titleEl) {
      document.title = lang === 'en'
        ? titleEl.getAttribute('data-title-en')
        : titleEl.getAttribute('data-title-fr');
    }

    // Boutons FR/EN desktop
    var btnFr = document.getElementById('btn-fr');
    var btnEn = document.getElementById('btn-en');
    if (btnFr) btnFr.classList.toggle('active', lang === 'fr');
    if (btnEn) btnEn.classList.toggle('active', lang === 'en');

    // Boutons FR/EN mobile
    var btnFrM = document.getElementById('btn-fr-mobile');
    var btnEnM = document.getElementById('btn-en-mobile');
    if (btnFrM) btnFrM.classList.toggle('active', lang === 'fr');
    if (btnEnM) btnEnM.classList.toggle('active', lang === 'en');

    document.documentElement.lang = lang;
  }

  window.setLangBtn = function(lang) {
    setLang(lang);
    applyLang(lang);
  };

  window.initLang = function() {
    applyLang(getLang());
  };

  // Init automatique
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', function() { window.initLang(); });
  } else {
    window.initLang();
  }
})();
