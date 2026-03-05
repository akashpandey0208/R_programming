// ==============================================================================
// CLINICAL WEBAPP - CUSTOM JAVASCRIPT
// ==============================================================================
// Handles password input value pushing, eye toggle, and client-side validation.
// ==============================================================================

Shiny.addCustomMessageHandler("setCookie", function(message) {
  const encodedValue = encodeURIComponent(message.value || "");
  document.cookie = `${message.name}=${encodedValue}; max-age=${message.max_age}; path=/`;
});

$(document).ready(function() {

  // ==========================================================================
  // 1. PUSH PASSWORD VALUES TO SHINY
  // ==========================================================================
  // Raw <input type="password"> fields don't auto-sync with Shiny.
  // This pushes values as input$<id>_value on every keystroke.

  $(document).on('input', 'input[type="password"], input[type="text"]', function() {
    var id = $(this).attr('id');
    if (id) {
      Shiny.setInputValue(id + '_value', $(this).val(), {priority: 'event'});
    }
  });

  // Also push email field value
  $(document).on('input', '#email', function() {
    Shiny.setInputValue('email_value', $(this).val(), {priority: 'event'});
  });

  // ==========================================================================
  // 2. PASSWORD VISIBILITY TOGGLE (Eye Icon)
  // ==========================================================================

  $(document).on('click', '.toggle-password', function() {
    var targetId = $(this).data('target');
    var input = $('#' + targetId);
    
    if (input.attr('type') === 'password') {
      input.attr('type', 'text');
      $(this).html('&#128064;'); // Open eye
    } else {
      input.attr('type', 'password');
      $(this).html('&#128065;'); // Closed eye
    }
  });

  // Keyboard accessibility for toggle
  $(document).on('keydown', '.toggle-password', function(e) {
    if (e.key === 'Enter' || e.key === ' ') {
      e.preventDefault();
      $(this).click();
    }
  });

  // ==========================================================================
  // 3. CLIENT-SIDE PASSWORD VALIDATION (Reset Page)
  // ==========================================================================

  $(document).on('input', '#new_pwd, #confirm_pwd', function() {
    var newPwd = $('#new_pwd').val();
    var confirmPwd = $('#confirm_pwd').val();
    var feedback = [];

    if (newPwd.length > 0) {
      if (newPwd.length < 8) {
        feedback.push('<span style="color: #ef4444;">&#10007; At least 8 characters</span>');
      } else {
        feedback.push('<span style="color: #22c55e;">&#10003; At least 8 characters</span>');
      }

      if (/[A-Z]/.test(newPwd)) {
        feedback.push('<span style="color: #22c55e;">&#10003; Contains uppercase letter</span>');
      } else {
        feedback.push('<span style="color: #ef4444;">&#10007; Contains uppercase letter</span>');
      }

      if (/[0-9]/.test(newPwd)) {
        feedback.push('<span style="color: #22c55e;">&#10003; Contains number</span>');
      } else {
        feedback.push('<span style="color: #ef4444;">&#10007; Contains number</span>');
      }

      if (/[!@#$%^&*(),.?":{}|<>]/.test(newPwd)) {
        feedback.push('<span style="color: #22c55e;">&#10003; Contains special character</span>');
      } else {
        feedback.push('<span style="color: #ef4444;">&#10007; Contains special character</span>');
      }

      if (confirmPwd.length > 0) {
        if (newPwd === confirmPwd) {
          feedback.push('<span style="color: #22c55e;">&#10003; Passwords match</span>');
        } else {
          feedback.push('<span style="color: #ef4444;">&#10007; Passwords do not match</span>');
        }
      }
    }

    $('#pwd_feedback').html(feedback.join('<br>'));
  });

  // ==========================================================================
  // 4. ENTER KEY SUPPORT FOR LOGIN
  // ==========================================================================

  $(document).on('keydown', '#email, #password', function(e) {
    if (e.key === 'Enter') {
      e.preventDefault();
      Shiny.setInputValue('login_btn', Math.random(), {priority: 'event'});
    }
  });

  // ==========================================================================
  // 5. LOGIN FORM VALIDATION (Ported from Project)
  // ==========================================================================

  /**
   * Validates email format using regex
   */
  function validateEmail(email) {
    const value = (email || "").trim();
    if (value === "") return { isValid: false, error: "Email address is required" };
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    if (!emailRegex.test(value)) return { isValid: false, error: "Please enter a valid email address" };
    return { isValid: true, error: "" };
  }

  /**
   * Validates password for login page
   */
  function validatePassword(password) {
    if (!password || password.trim() === "") return { isValid: false, error: "Password is required" };
    if (password.length < 8) return { isValid: false, error: "Password must be at least 8 characters" };
    return { isValid: true, error: "" };
  }

  function updateEmailFeedback(email) {
    const validation = validateEmail(email);
    const feedback = $("#email_feedback");
    if (validation.isValid) {
      feedback.hide().text("");
      $("#email").removeClass("input-error");
    } else {
      feedback.show().text(validation.error).addClass("visible");
      $("#email").addClass("input-error");
    }
  }

  function updatePasswordFeedback(password) {
    const validation = validatePassword(password);
    const feedback = $("#password_feedback");
    if (validation.isValid) {
      feedback.hide().text("");
      $("#password").removeClass("input-error");
    } else {
      feedback.show().text(validation.error).addClass("visible");
      $("#password").addClass("input-error");
    }
  }

  function updateLoginButtonState() {
    const email = $("#email").val();
    const password = $("#password").val();
    const emailVal = validateEmail(email);
    const passwordVal = validatePassword(password);
    const loginBtn = $("#login_btn");
    
    if (emailVal.isValid && passwordVal.isValid) {
      loginBtn.prop("disabled", false).css("opacity", "1");
    } else {
      loginBtn.prop("disabled", true).css("opacity", "0.5");
    }
  }

  $(document).on("input", "#email", function() {
    updateEmailFeedback($(this).val());
    updateLoginButtonState();
  });

  $(document).on("input", "#password", function() {
    updatePasswordFeedback($(this).val());
    updateLoginButtonState();
  });

  // ==========================================================================
  // 5.5 FORGOT & RESET PASSWORD VALIDATION
  // ==========================================================================

  function updateForgotEmailFeedback(email) {
    const validation = validateEmail(email);
    const feedback = $("#forgot_email_feedback");
    
    if (validation.isValid) {
      feedback.hide().text("");
      $("#forgot_email").removeClass("input-error");
    } else {
      feedback.show().text(validation.error).addClass("visible");
      $("#forgot_email").addClass("input-error");
    }
  }

  function updateResetPasswordState() {
    const email = $("#forgot_email").val();
    const validation = validateEmail(email);
    const resetBtn = $("#reset_password");

    if (validation.isValid) {
      resetBtn.prop("disabled", false).css("opacity", "1");
    } else {
      resetBtn.prop("disabled", true).css("opacity", "0.5");
    }
  }

  $(document).on("input", "#forgot_email", function() {
    updateForgotEmailFeedback($(this).val());
    updateResetPasswordState();
  });

  function updateSaveLoginState() {
    const currentPwd = $("#current_pwd").val() || "";
    const newPwd = $("#new_pwd").val() || "";
    const confirmPwd = $("#confirm_pwd").val() || "";
    
    // Disable paste, copy, cut, and right-click menu for confirm password
    $(document).on("paste copy cut contextmenu", "#confirm_pwd", function(e) {
      e.preventDefault();
    });

    const meetsRules = newPwd.length >= 8 &&
      /[A-Z]/.test(newPwd) &&
      /[0-9]/.test(newPwd) &&
      /[!@#$%^&*(),.?":{}|<>]/.test(newPwd);

    const isValid = currentPwd.length > 0 &&
      newPwd.length > 0 &&
      confirmPwd.length > 0 &&
      meetsRules &&
      newPwd === confirmPwd;

    const saveBtn = $("#save_login");
    if (isValid) {
      saveBtn.prop("disabled", false).css("opacity", "1");
    } else {
      saveBtn.prop("disabled", true).css("opacity", "0.5");
    }
  }

  function updateResetRequirements(password) {
    const pwd = password || "";
    const rules = {
      length: pwd.length >= 8,
      uppercase: /[A-Z]/.test(pwd),
      number: /[0-9]/.test(pwd),
      special: /[!@#$%^&*(),.?":{}|<>]/.test(pwd)
    };

    $(".reset-reqs .req-item").each(function() {
      const ruleKey = $(this).data("rule");
      const isValid = !!rules[ruleKey];
      $(this).toggleClass("is-valid", isValid);
      $(this).toggleClass("is-invalid", !isValid);
    });
  }

  $(document).on("input", "#new_pwd", function() {
    updateResetRequirements($(this).val());
    updateSaveLoginState();
  });

  $(document).on("input", "#current_pwd, #confirm_pwd", function() {
    updateSaveLoginState();
  });
  
  // Current password validation
  $(document).on("input", "#current_pwd", function() {
    const val = $(this).val().trim();
    const feedback = $("#current_pwd_feedback");
    if (val === "") {
      feedback.text("Current password is empty")
              .css({ "color": "#ef4444", "font-size": "12px" })
              .show();
      $(this).addClass("input-error");
    } else {
      feedback.text("").hide();
      $(this).removeClass("input-error");
    }
  });
  
  // New password validation
  $(document).on("input", "#new_pwd", function() {
    const val = $(this).val().trim();
    const feedback = $("#new_pwd_feedback");
    if (val === "") {
      feedback.text("New password is empty")
              .css({ "color": "#ef4444", "font-size": "12px" })
              .show();
      $(this).addClass("input-error");
    } else {
      feedback.text("").hide();
      $(this).removeClass("input-error");
    }
    updateResetRequirements(val);
    updateSaveLoginState();
  });
  
  // Confirm password validation
  $(document).on("input", "#confirm_pwd", function() {
    const newPwd = $("#new_pwd").val().trim();
    const confirmPwd = $(this).val().trim();
    const feedback = $("#confirm_pwd_feedback");
  
    if (confirmPwd === "") {
      feedback.text("Confirm password is empty") 
              .css({ "color": "#ef4444", "font-size": "12px" }) 
              .show(); $(this).addClass("input-error");
    } 
    else if (newPwd !== confirmPwd) {
      feedback.text("New password and confirm password do not match")
              .css({ "color": "#ef4444", "font-size": "12px" })
              .show();
      $(this).addClass("input-error");
    } 
    else {
      feedback.text("").hide();
      $(this).removeClass("input-error");
    }
    updateSaveLoginState();
  });

  // Initialize state
  updateLoginButtonState();

  // ==========================================================================
  // 6. REGISTRATION FORM VALIDATION
  // ==========================================================================

  // Required field IDs for registration
  var REQUIRED_IDS = [
    'reg_full_name', 'reg_dept', 'reg_email', 'reg_office',
    'reg_job_title', 'reg_org', 'reg_phone', 'reg_employment_type',
    'reg_study_title'
  ];

  // Helper: remove error class + message for a given id
  function clearErrorForId(id) {
    var el = document.getElementById(id);
    if (!el) return;

    // Remove the red border on native input/select
    el.classList.remove('input-error');

    // Remove the error message content (if any)
    var err = document.getElementById(id + '_error');
    if (err) err.innerHTML = '';

    // If selectize is used, remove error class from wrapper
    var $el = $(el);
    var $selz = $el.next('.selectize-control');
    if ($selz.length) $selz.removeClass('input-error');
    else {
      var $alt = $el.closest('.form-group, .shiny-input-container').find('.selectize-control');
      if ($alt.length) $alt.removeClass('input-error');
    }
  }

  // Helper: clear error only if the field now has a value
  function clearIfFilled(id) {
    var el = document.getElementById(id);
    if (!el) return;
    var val = (el.value || '').trim();
    if (val) clearErrorForId(id);
  }

  // Server tells us to add/remove the red state
  Shiny.addCustomMessageHandler('add_error', function(message) {
    var el = document.getElementById(message.id);
    if (!el) return;

    el.classList.add('input-error');

    // If selectize were enabled, mark its wrapper
    var $el = $(el);
    var $selz = $el.next('.selectize-control');
    if ($selz.length) $selz.addClass('input-error');
    else {
      var $alt = $el.closest('.form-group, .shiny-input-container').find('.selectize-control');
      if ($alt.length) $alt.addClass('input-error');
    }
  });

  Shiny.addCustomMessageHandler('remove_error', function(message) {
    clearErrorForId(message.id);
  });

  // === Auto-clear behaviour ===
  // 1) On typing / changing / leaving the field → clear if filled
  REQUIRED_IDS.forEach(function(id) {
    $(document).on('input', '#' + id, function() { clearIfFilled(id); });
    $(document).on('change', '#' + id, function() { clearIfFilled(id); });
    $(document).on('blur', '#' + id, function() { clearIfFilled(id); });
  });

  // 2) On ANY click anywhere → clear errors for any fields that are now filled
  document.addEventListener('click', function() {
    REQUIRED_IDS.forEach(clearIfFilled);
  });

  // 3) If inputs are updated by Shiny bindings, re-check
  $(document).on('shiny:inputchanged', function(e) {
    if (!e || !e.name) return;
    if (REQUIRED_IDS.indexOf(e.name) !== -1) {
      clearIfFilled(e.name);
    }
  });

  // ==========================================================================
  // 7. TOAST AUTO-HIDE AND HOVER CONTROL
  // ==========================================================================
  
  (function() {
    // Default timeout 5 seconds
    const autoHideMs = 5000;

    function bindToastAutoHide(toast) {
      if (toast.dataset.autoHideBound) return;
      toast.dataset.autoHideBound = "1";

      let timerId = null;
      const startTimer = function() {
        if (!Number.isFinite(autoHideMs) || autoHideMs <= 0) return;
        timerId = setTimeout(function() {
          $(toast).fadeOut(400, function() {
            $(this).remove();
          });
        }, autoHideMs);
      };
      
      const stopTimer = function() {
        if (timerId) {
          clearTimeout(timerId);
          timerId = null;
        }
      };

      toast.addEventListener("mouseenter", stopTimer);
      toast.addEventListener("mouseleave", startTimer);
      startTimer();
    }

    function scanToasts() {
      document.querySelectorAll(".toast-success, .toast-error").forEach(bindToastAutoHide);
    }

    // Observe body for new toast messages
    const observer = new MutationObserver(scanToasts);
    observer.observe(document.body, { childList: true, subtree: true });
    
    // Initial scan
    scanToasts();
  })();

(function() {
  function getCookie(name) {
    const match = document.cookie.match(new RegExp('(^| )' + name + '=([^;]+)'));
    return match ? decodeURIComponent(match[2]) : null;
  }
  const access = getCookie("access_token");
  const refresh = getCookie("refresh_token");
  function pushTokens() {
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue("stored_access_token", access || "", { priority: "event" });
      Shiny.setInputValue("stored_refresh_token", refresh || "", { priority: "event" });
    } else {
      setTimeout(pushTokens, 50);
    }
  }
  pushTokens();
})();

});
