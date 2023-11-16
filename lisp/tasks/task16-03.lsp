(print 
  (let ((L '((((A) (NIL) ((NIL) NIL (B (C D) E F) (X)))) G (NIL) H)))
    (caar
      (cdddar
        (cddaar L)))))