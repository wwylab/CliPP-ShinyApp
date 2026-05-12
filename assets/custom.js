// ============================================================================
// CliPP Custom JavaScript - Enhanced Interactivity
// ============================================================================

// Page metadata used by the sidebar nav to update title, subtitle, and breadcrumb.
window.pageHeaders = {
  "About":               ["CliPP",               "Clonal Structure Identification through Pairwise Penalization"],
  "Tutorial":            ["Tutorial",            "Step-by-step guide to using CliPP on Web"],
  "CliPP-on-Web":        ["CliPP-on-Web",        "Upload your own samples for fast subclonal reconstruction"],
  "CliPP Data Resources":["CliPP Data Resources","Explore subclonal reconstruction results for TCGA and PCAWG samples"],
  "Driver Mutation":     ["Driver Mutation",     "Driver mutation annotations across TCGA cancer types"],
  "Downloads":           ["Downloads",           "Download datasets, simulation data, and pipeline outputs"]
};

// ---- Citation helpers ----
window.copyTextToClipboard = function(text, successMessage) {
  var done = function() {
    if (window.showNotification) showNotification(successMessage || 'Copied', 'success');
  };
  if (navigator.clipboard && navigator.clipboard.writeText) {
    navigator.clipboard.writeText(text).then(done).catch(function() {
      // Fallback to legacy
      var ta = document.createElement('textarea');
      ta.value = text; document.body.appendChild(ta);
      ta.select(); document.execCommand('copy'); document.body.removeChild(ta);
      done();
    });
  } else {
    var ta = document.createElement('textarea');
    ta.value = text; document.body.appendChild(ta);
    ta.select(); document.execCommand('copy'); document.body.removeChild(ta);
    done();
  }
};

window.copyBibtex = function() {
  var el = document.getElementById('bibtex-content');
  if (el) copyTextToClipboard(el.innerText, 'BibTeX copied');
};

// ---- Tutorial chapter seek ----
window.seekTutorial = function(el) {
  var sec = parseInt(el.getAttribute('data-time'), 10) || 0;
  var iframe = document.getElementById('tutorial-iframe');
  if (iframe) {
    var base = iframe.src.split('?')[0];
    iframe.src = base + '?start=' + sec + '&autoplay=1&enablejsapi=1&rel=0&modestbranding=1';
  }
  document.querySelectorAll('.chapter-row').forEach(function(r) { r.classList.remove('active'); });
  el.classList.add('active');
};

window.downloadCitation = function(format) {
  var ris = [
    'TY  - JOUR',
    'TI  - Subclonal mutational load predicts survival and response to immunotherapy in cancers with low to moderate TMB',
    'AU  - Jiang, Yujie',
    'AU  - Montierth, Matthew D',
    'AU  - Yu, Kaixian',
    'AU  - Ji, Shuangxi',
    'AU  - Tran, Quang',
    'AU  - Liu, Xiaoqian',
    'AU  - Lal, Jessica C',
    'AU  - Guo, Shuai',
    'AU  - Wu, Aaron',
    'AU  - Shin, Seung Jun',
    'AU  - Cao, Shaolong',
    'AU  - Li, Ruonan',
    'AU  - Tang, Yuxin',
    'AU  - Lesluyes, Tom',
    'AU  - Kopetz, Scott',
    'AU  - Msaouel, Pavlos',
    'AU  - Sood, Anil K',
    'AU  - Jones, Christopher',
    'AU  - Ajani, Jaffer',
    'AU  - Subudhi, Sumit K',
    'AU  - Aparicio, Ana',
    'AU  - Sharma, Padmanee',
    'AU  - Shen, John Paul',
    'AU  - Kimmel, Marek',
    'AU  - Wang, Jennifer R',
    'AU  - Tarabichi, Maxime',
    'AU  - Fitzgerald, Rebecca',
    'AU  - Van Loo, Peter',
    'AU  - Zhu, Hongtu',
    'AU  - Wang, Wenyi',
    'JO  - bioRxiv',
    'PY  - 2024',
    'DO  - 10.1101/2024.07.03.601939',
    'UR  - https://www.biorxiv.org/content/10.1101/2024.07.03.601939v1',
    'ER  - '
  ].join('\n');

  var enw = [
    '%0 Journal Article',
    '%T Subclonal mutational load predicts survival and response to immunotherapy in cancers with low to moderate TMB',
    '%A Jiang, Yujie',
    '%A Montierth, Matthew D',
    '%A Yu, Kaixian',
    '%A Ji, Shuangxi',
    '%A Tran, Quang',
    '%A Liu, Xiaoqian',
    '%A Lal, Jessica C',
    '%A Guo, Shuai',
    '%A Wu, Aaron',
    '%A Shin, Seung Jun',
    '%A Cao, Shaolong',
    '%A Li, Ruonan',
    '%A Tang, Yuxin',
    '%A Lesluyes, Tom',
    '%A Kopetz, Scott',
    '%A Msaouel, Pavlos',
    '%A Sood, Anil K',
    '%A Jones, Christopher',
    '%A Ajani, Jaffer',
    '%A Subudhi, Sumit K',
    '%A Aparicio, Ana',
    '%A Sharma, Padmanee',
    '%A Shen, John Paul',
    '%A Kimmel, Marek',
    '%A Wang, Jennifer R',
    '%A Tarabichi, Maxime',
    '%A Fitzgerald, Rebecca',
    '%A Van Loo, Peter',
    '%A Zhu, Hongtu',
    '%A Wang, Wenyi',
    '%J bioRxiv',
    '%D 2024',
    '%R 10.1101/2024.07.03.601939',
    '%U https://www.biorxiv.org/content/10.1101/2024.07.03.601939v1'
  ].join('\n');

  var content, filename, mime;
  if (format === 'ris') { content = ris; filename = 'clipp.ris'; mime = 'application/x-research-info-systems'; }
  else                  { content = enw; filename = 'clipp.enw'; mime = 'application/x-endnote-refer'; }

  var blob = new Blob([content], { type: mime });
  var url = URL.createObjectURL(blob);
  var a = document.createElement('a');
  a.href = url; a.download = filename;
  document.body.appendChild(a); a.click();
  setTimeout(function() { document.body.removeChild(a); URL.revokeObjectURL(url); }, 100);
};

$(document).ready(function() {
  console.log('CliPP Enhanced UI Loaded');

  // ============================================================================
  // Sidebar Navigation
  // ============================================================================

  $(document).on('click', '.sidebar-link', function(e) {
    e.preventDefault();
    var $link = $(this);
    var tab = $link.data('tab');
    if (!tab) return;

    // Tell server to switch the hidden tabsetPanel
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue('main_nav_target', tab, { priority: 'event' });
    }

    // Active state
    $('.sidebar-link').removeClass('active');
    $link.addClass('active');

    // Breadcrumb + page header
    var meta = window.pageHeaders[tab];
    if (meta) {
      $('.breadcrumb-current').text(meta[0]);
      $('.page-title').text(meta[0]);
      $('.page-subtitle').text(meta[1]);
    } else {
      $('.breadcrumb-current').text(tab);
      $('.page-title').text(tab);
      $('.page-subtitle').text('');
    }

    // Scroll the page back to the top on tab change
    window.scrollTo({ top: 0, left: 0, behavior: 'auto' });
  });

  // ============================================================================
  // Smooth Scroll for Navigation
  // ============================================================================

  $('a[href^="#"]').on('click', function(e) {
    const target = $(this.getAttribute('href'));
    if (target.length) {
      e.preventDefault();
      $('html, body').stop().animate({
        scrollTop: target.offset().top - 80
      }, 800);
    }
  });

  // ============================================================================
  // Add Fade-in Animation to Elements
  // ============================================================================

  function addFadeInAnimation() {
    const elements = $('.card, .plot-container, .well');
    elements.each(function(index) {
      $(this).css({
        'opacity': '0',
        'transform': 'translateY(20px)'
      });

      setTimeout(() => {
        $(this).css({
          'transition': 'all 0.5s ease',
          'opacity': '1',
          'transform': 'translateY(0)'
        });
      }, index * 100);
    });
  }

  // Run on page load
  setTimeout(addFadeInAnimation, 100);

  // ============================================================================
  // Enhanced Button Click Effects
  // ============================================================================

  $('.btn').on('click', function(e) {
    const button = $(this);

    // Add ripple effect
    const ripple = $('<span class="ripple"></span>');
    button.append(ripple);

    const x = e.pageX - button.offset().left;
    const y = e.pageY - button.offset().top;

    ripple.css({
      left: x,
      top: y
    });

    setTimeout(() => ripple.remove(), 600);
  });

  // ============================================================================
  // Plot Loading Indicators
  // ============================================================================

  // Add loading spinner to plots
  Shiny.addCustomMessageHandler('showPlotLoading', function(outputId) {
    const plotDiv = $('#' + outputId);
    if (plotDiv.length && !plotDiv.find('.loading-spinner').length) {
      plotDiv.prepend(`
        <div class="loading-spinner" style="
          position: absolute;
          top: 50%;
          left: 50%;
          transform: translate(-50%, -50%);
          z-index: 1000;
        ">
          <div class="spinner-border text-primary" role="status">
            <span class="sr-only">Loading...</span>
          </div>
        </div>
      `);
    }
  });

  Shiny.addCustomMessageHandler('hidePlotLoading', function(outputId) {
    $('#' + outputId + ' .loading-spinner').fadeOut(300, function() {
      $(this).remove();
    });
  });

  // ============================================================================
  // Auto-hide Success Notifications
  // ============================================================================

  $(document).on('shiny:notification', function(event) {
    if (event.notification.type === 'message') {
      setTimeout(() => {
        $(event.notification.el).fadeOut(300, function() {
          $(this).remove();
        });
      }, 3000);
    }
  });

  // ============================================================================
  // Enhanced File Upload Display
  // ============================================================================

  $('input[type="file"]').on('change', function() {
    const fileName = $(this).val().split('\\').pop();
    const fileDisplay = $(this).siblings('.file-upload-display');

    if (fileDisplay.length) {
      fileDisplay.text(fileName || 'No file chosen');
    } else {
      $(this).after(`
        <div class="file-upload-display" style="
          margin-top: 0.5rem;
          padding: 0.5rem;
          background: var(--gray-100);
          border-radius: var(--radius-md);
          font-size: 0.875rem;
          color: var(--gray-600);
        ">
          ${fileName}
        </div>
      `);
    }
  });

  // ============================================================================
  // Tooltips Enhancement
  // ============================================================================

  function initializeTooltips() {
    $('[data-toggle="tooltip"]').tooltip({
      trigger: 'hover',
      animation: true,
      delay: { show: 500, hide: 100 }
    });
  }

  initializeTooltips();

  // Reinitialize tooltips when content changes
  $(document).on('shiny:value', function() {
    setTimeout(initializeTooltips, 100);
  });

  // ============================================================================
  // Collapsible Sections
  // ============================================================================

  $('.collapsible-header').on('click', function() {
    const content = $(this).next('.collapsible-content');
    const icon = $(this).find('.collapse-icon');

    content.slideToggle(300);
    icon.toggleClass('rotated');
  });

  // ============================================================================
  // Smooth Number Counter Animation
  // ============================================================================

  function animateValue(element, start, end, duration) {
    const range = end - start;
    const increment = range / (duration / 16);
    let current = start;

    const timer = setInterval(function() {
      current += increment;
      if ((increment > 0 && current >= end) || (increment < 0 && current <= end)) {
        current = end;
        clearInterval(timer);
      }
      element.text(Math.round(current));
    }, 16);
  }

  // ============================================================================
  // Progress Bar Animation
  // ============================================================================

  function animateProgressBar(selector, targetPercent, duration = 1000) {
    const progressBar = $(selector);
    let currentPercent = 0;
    const increment = targetPercent / (duration / 16);

    const timer = setInterval(function() {
      currentPercent += increment;
      if (currentPercent >= targetPercent) {
        currentPercent = targetPercent;
        clearInterval(timer);
      }
      progressBar.css('width', currentPercent + '%');
      progressBar.attr('aria-valuenow', Math.round(currentPercent));
    }, 16);
  }

  // ============================================================================
  // Table Row Highlighting
  // ============================================================================

  $(document).on('mouseenter', '.dataTable tbody tr', function() {
    $(this).css({
      'background-color': 'var(--gray-50)',
      'transform': 'scale(1.01)',
      'box-shadow': 'var(--shadow-sm)'
    });
  });

  $(document).on('mouseleave', '.dataTable tbody tr', function() {
    $(this).css({
      'background-color': '',
      'transform': '',
      'box-shadow': ''
    });
  });

  // ============================================================================
  // Enhanced Plotly Interactions
  // ============================================================================

  $(document).on('plotly_afterplot', function(event, data) {
    const plotDiv = data[0];
    $(plotDiv).css({
      'opacity': '0',
      'transform': 'scale(0.95)'
    });

    setTimeout(() => {
      $(plotDiv).css({
        'transition': 'all 0.5s ease',
        'opacity': '1',
        'transform': 'scale(1)'
      });
    }, 50);
  });

  // ============================================================================
  // Dark Mode Toggle (Optional Feature)
  // ============================================================================

  window.toggleDarkMode = function() {
    $('body').toggleClass('dark-mode');
    const isDark = $('body').hasClass('dark-mode');
    localStorage.setItem('darkMode', isDark);
  };

  // Load dark mode preference
  if (localStorage.getItem('darkMode') === 'true') {
    $('body').addClass('dark-mode');
  }

  // ============================================================================
  // Keyboard Shortcuts
  // ============================================================================

  $(document).on('keydown', function(e) {
    // Ctrl/Cmd + K: Focus on search/input
    if ((e.ctrlKey || e.metaKey) && e.key === 'k') {
      e.preventDefault();
      $('input[type="text"]:visible:first').focus();
    }

    // Ctrl/Cmd + Enter: Submit/Analyze
    if ((e.ctrlKey || e.metaKey) && e.key === 'Enter') {
      e.preventDefault();
      $('.btn-primary:visible:first').click();
    }
  });

  // ============================================================================
  // Copy to Clipboard Functionality
  // ============================================================================

  window.copyToClipboard = function(text) {
    const tempInput = $('<textarea>');
    $('body').append(tempInput);
    tempInput.val(text).select();
    document.execCommand('copy');
    tempInput.remove();

    // Show success message
    showNotification('Copied to clipboard!', 'success');
  };

  // ============================================================================
  // Custom Notification System
  // ============================================================================

  window.showNotification = function(message, type = 'info') {
    const colors = {
      success: '#10b981',
      error: '#ef4444',
      warning: '#f59e0b',
      info: '#3b82f6'
    };

    const notification = $(`
      <div class="custom-notification" style="
        position: fixed;
        top: 20px;
        right: 20px;
        background: white;
        padding: 1rem 1.5rem;
        border-radius: var(--radius-lg);
        box-shadow: var(--shadow-xl);
        border-left: 4px solid ${colors[type]};
        z-index: 9999;
        opacity: 0;
        transform: translateX(100%);
        transition: all 0.3s ease;
      ">
        ${message}
      </div>
    `);

    $('body').append(notification);

    setTimeout(() => {
      notification.css({
        'opacity': '1',
        'transform': 'translateX(0)'
      });
    }, 10);

    setTimeout(() => {
      notification.css({
        'opacity': '0',
        'transform': 'translateX(100%)'
      });
      setTimeout(() => notification.remove(), 300);
    }, 3000);
  };

  // ============================================================================
  // Initialize All Features
  // ============================================================================

  console.log('CliPP Enhanced Features Initialized:');
  console.log('✓ Smooth scrolling');
  console.log('✓ Fade-in animations');
  console.log('✓ Enhanced buttons');
  console.log('✓ Loading indicators');
  console.log('✓ Keyboard shortcuts (Ctrl+K, Ctrl+Enter)');
  console.log('✓ Custom notifications');

});
