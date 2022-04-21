var keysdown = '';

check_sequence = function(e) {
  keysdown += e.key; 
  if (/shiny$/.test(keysdown)) {
    Shiny.setInputValue('hidden_mode', true);
  }
  if (/modeoff$/.test(keysdown)) {
    Shiny.setInputValue('hidden_mode', false);
  }
  if (!/s$|h$|i$|n$|y$|m$|o$|d$|e$|f$/.test(keysdown)) {
    keysdown = '';
  }
};

document.addEventListener('keydown', check_sequence);
