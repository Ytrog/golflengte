					; calculate the frequency of light depending on wavelength


					; the formula for frequency: F = c/lambda, where F is hertz, c is speed of light in m/s, and lambda is in meters

					; Some helpful links: https://www.reddit.com/r/Common_Lisp/comments/1bk3xoq/is_this_a_good_use_of_the_type_system/
; https://google.github.io/styleguide/lispguide.xml

					; type declarations

(defun positive-real-p (number)
  "Determine if the parameter is a positive number"
  (and (realp number)
       (> number 0)))

(deftype positive-real ()
  `(satisfies positive-real-p))

; constants

(defconstant +c+ 299792458 "The speed of light in a vacuum")

					; utils

(defmacro swallow (&body body)
  "Swallow the return value of the <body>"
  `(progn
     ,@body
     (values)))

; calculations

(defun frequency-from-wavelength (lambda) ; todo use condition system
  "Calculate the frequency in hertz from wavelengt <lambda> in meters"
  ; (declare (type positive-real lambda)) ; seems to prevent (check-type ...) from working
  (check-type lambda positive-real)
  (when (zerop lambda) (error 'division-by-zero))
  (unless (plusp lambda) (error 'arithmetic-error :operation 'frequency-from-wavelength :operands `(,lambda)))
  (/ +c+ lambda))

(defun frequency-from-wavelength-f (lambda)
  (format t "~& The frequency of light with a wavelength of ~d meters is ~d hertz" lambda (frequency-from-wavelength lambda)))

(defun wavelength-from-frequency (freq)
  (check-type freq positive-real)
  (when (zerop freq) (error 'division-by-zero))
  (unless (plusp freq) (error 'arithmetic-error :operation 'wavelength-from-frequency :operands `(,freq)))
  "Calculate the wavelenght in meters from the frequency in hertz"
  (/ +c+ freq))

; UI

; todo prevent newline after prompt
(defun prompt (&optional (p "> "))
  "Display a prompt"
  (swallow
    (format t "~&~a" p)))

(defmacro ask (question f answer-form)
  "Ask a <question> using function <f> and print the answer using <answer-form>"
  (check-type question string)
  (check-type answer-form string)
  (let ((in (gensym "in"))
	(out (gensym "out")))
    `(progn
       (format t ,question)
       (let* ((,in (read-int))
	      (,out (,f ,in)))
	 (format t ,answer-form ,out)))))
	      

(defun read-int ()
  "Read line and parse to int"
  (prompt)
  (let ((input (read-line)))
    (parse-integer input :junk-allowed t)))

(defmacro options (&rest options-list)
  "Define the options with each option being in the form (1 \"Menu option text\" (form-if-chosen))"
  `(progn
     ,@(loop for o in options-list
	     do (unless (= (length o) 3) (error "List ~a is not of length 3~2%" 'o)) ; todo better error type?
	     collect `(format t "~&~4t~d~4t~a" ,(first o) ,(second o)))
     (terpri)
     (let ((input (read-int)))
       (cond
	 ((null input) (format t "Geen geldig getal"))
	 ,@(loop for o in options-list
		 collect `((= input ,(first o)) ,(third o)))
	 (t (format t "~&Onbekende optie: ~a" input))))))

(defun menu-frequency ()
  "Open menu for frequency calculation"
  (format t "~&In welke eenheid is de golflengte?")
  (options
   (1 "Meters"  (ask "Golflengte (m): " frequency-from-wavelength "Frequency: ~g hertz~%"))
   (2 "mm" (ask "Golflengte (mm): "
		(lambda (x)
		  (frequency-from-wavelength (/ x 1000)))
		"Frequency: ~g hertz~%"))))


(defun menu-wavelength ()
  "Open menu for wavelength calculation"
  (format t "~&In welke eenheid wil je de golflengte hebben?~%")
  (options
   (1 "Meters" (ask "Frequency: " wavelength-from-frequency "Wavelenght: ~g meter~%"))))

(defun golflengte ()
  "Main menu of application"
  (loop
    (format t  "Welkom bij de golflengte berekenaar~%Wat wilt u doen?:~%")
    (options
     (1 "Bereken frequentie van golflengte" (menu-frequency))
     (2 "Bereken golflengte van frequentie" (menu-wavelength))
     (0 "Exit" (return))))) 
     
