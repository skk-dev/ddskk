(require 'ert)

(require 'skk)


(ert-deftest skk-compute-henkan-lists/test1 ()
  (let ((fixtures
         '(("てんさい /転載/天災/天才/" nil
            (("転載" "天災" "天才") nil nil nil))
           ("なk /亡/無/鳴/泣/[く/無/鳴/泣/]/[き/亡/]/" "く"
            (("亡" "無" "鳴" "泣") ("[く") ("無" "鳴" "泣") ("]" "[き" "亡" "]"))))))
    (dolist (f fixtures)
      (let ((entry (car f))
            (okurigana (nth 1 f))
            (expected  (nth 2 f)))
        (with-temp-buffer
          (insert ";;\n")
          (insert entry)
          (insert "\n")
          (forward-line -1)
          (search-forward "/" nil 'noerror)
          (should (equal (skk-compute-henkan-lists okurigana)
                         expected)))))))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-test.el ends here
