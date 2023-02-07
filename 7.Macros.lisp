(defmacro nil! (var)
  (list 'setq var nil))
