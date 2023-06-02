;;; ai-blog.el --- Blog post generation using ChatGPT

;; Author: Henrique Goncalves <kamus@hadenes.io>
;; URL: https://github.com/kamushadenes
;; Version: 0.0.1

;;; Commentary:
;;
;; Blog post generation using ChatGPT
;;
;;; Code:

(require 'gptel)
(require 'f)
(require 'easy-hugo)
(require 'url)
(require 'json)
(require 'thingatpt)

(defgroup ai-blog nil
  "Blog post generation using ChatGPT."
  :group 'ai-blog)

(defcustom ai-blog-dall-e-api-key nil
  "The API key to use for the DALL-E API."
  :type 'string
  :group 'ai-blog)

(defcustom ai-blog-pexels-api-key nil
  "The API key to use for the pexels API."
  :type 'string
  :group 'ai-blog)

(defcustom ai-blog-featured-image-field-name "image"
  "The field name to use for the featured image."
  :type 'string
  :group 'ai-blog)

(defcustom ai-blog-dall-e-url "https://api.openai.com/v1/images/generations"
  "The URL to use for the DALL-E API."
  :type 'string
  :group 'ai-blog)

(defcustom ai-blog-pexels-url "https://api.pexels.com/v1/search"
  "The URL to use for the pexels API."
  :type 'string
  :group 'ai-blog)

(defcustom ai-blog-dall-e-image-count 5
  "The number of images to generate with DALL-E."
  :type 'integer
  :group 'ai-blog)

(defcustom ai-blog-pexels-image-count 10
  "The number of images to retrieve from Pexels."
  :type 'integer
  :group 'ai-blog)


(defcustom ai-blog-image-buffer-name "*ai-blog-image*"
  "The name of the buffer to use for generating images."
  :type 'string
  :group 'ai-blog)

(defcustom ai-blog-current-persona "example"
  "The current persona to use for generating blog posts."
  :type 'string
  :group 'ai-blog)

(defun ai-blog-strip-persona (persona)
  "Strip the persona suffix from PERSONA."
  (substring persona 0 (- (length persona) 7)))

(defun ai-blog-get-persona-system (persona)
  "Get the system prompt for PERSONA."
  (f-read-text (concat "~/.config/doom/functions/ai-blog/personas/" persona ".system")))

(defun ai-blog-get-persona-prompt (persona)
  "Get the user prompt for PERSONA."
  (f-read-text (concat "~/.config/doom/functions/ai-blog/personas/" persona ".prompt")))

(defun ai-blog-select-persona ()
  "Select the persona to use for generating blog posts."
  (interactive)
  (let* ((files (directory-files "~/.config/doom/functions/ai-blog/personas/" nil ".*\.system"))
         (personas (mapcar 'ai-blog-strip-persona files))
         (persona (completing-read "Persona: " personas nil t)))
    (setq ai-blog-current-persona persona)
    (message "Persona selected: %s" ai-blog-current-persona)))

(defun ai-blog-get-current-persona ()
  "Get the current persona to use for generating blog posts."
  (ai-blog-get-persona-system ai-blog-current-persona))

(defun ai-blog-get-current-prompt ()
  "Get the current prompt to use for generating blog posts."
  (ai-blog-get-persona-prompt ai-blog-current-persona))

;;;###autoload
(defun ai-blog-generate-tags ()
  "Generate tags using ChatGPT."
  (interactive)
  (let ((content (buffer-substring-no-properties (point-min) (point-max))))
    (message "Generating tags...")
    (gptel-request
     (concat "Content: " content)
     :buffer (current-buffer)
     :system "Your task is to generate SEO-friendly tags. Reply in the following format: `tags: [\"tag1\", \"tag2\", \"tag3\"]`"
     :callback
     (lambda (response info)
       (if (not response)
           (message "Failed to generate tags! Error: %s" (plist-get info :status))
         (let* ((buf (plist-get info :buffer)))
           (with-current-buffer buf
             (goto-char (point-min))
             (forward-line 1)
             (recenter-top-bottom)
             (insert response "\n")
             (message "Tags generated!"))))))))

;;;###autoload
(defun ai-blog-write-post (title)
  "Write a blog post using ChatGPT. TITLE is the title of the blog post."
  (interactive "sTitle: ")
  (when (string= title "") (user-error "Title cannot be empty"))
  (if (not (boundp 'ai-blog-current-persona))
      (ai-blog-select-persona))
  (let ((system-prompt (ai-blog-get-current-persona))
        (user-prompt (concat (ai-blog-get-current-prompt) title)))
    (message "Generating blog post...")
    (set-window-point
     (get-buffer-window (current-buffer))
     (point-max))
    (recenter-top-bottom)
    (gptel-request
     user-prompt
     :buffer (current-buffer)
     :system system-prompt
     :stream t
     :callback
     (lambda (response info)
       (if (not response)
           (message "Failed to generate blog post! Error: %s" (plist-get info :status))
         (let* ((buf (plist-get info :buffer)))
           (with-current-buffer buf
             (save-excursion
               (goto-char (point-max))
               (insert response)))))))))

;; image generation

(defun ai-blog--dall-e-get-request-args-list (params)
  "Get the request args list for the DALL-E API using PARAMS."
  (concat "?"
	  (combine-and-quote-strings
	   (mapcar (lambda(x) (concat (substring (symbol-name (car x)) 1) "=" (cadr x))) (seq-partition params 2))
	   "&")))

(defun ai-blog--send-json-request (url method params token)
  "Send a METHOD request to the specified URL passing PARAMS and authenticating with TOKEN."
  (let* ((url-request-method method)
         (url-request-extra-headers `(("Authorization" . ,token)
                                      ("Content-Type" . "application/json")))
         (url-request-data (json-encode params))
	 (url (if (equal method "GET")
		  (concat url "/" (ai-blog--dall-e-get-request-args-list params))
		url))
         (buffer (url-retrieve-synchronously url t)))
    (if (not buffer)
        (error "Failed to send request to %s" url))
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "^$")
      (json-read))))

(defun ai-blog--resolve-secret (secret)
  "Resolve SECRET. If SECRET is a function, call it and return the result. Otherwise, return SECRET."
  (if (functionp secret)
      (funcall secret)
    secret))

(defun ai-blog--dall-e-request-images (prompt n)
  "Request N images from the DALL-E API using PROMPT."
  (unless ai-blog-dall-e-api-key
    (error "DALL-E API key not set"))
  (message "Generating images with DALL-E...")
  (let* ((params (list
                  :prompt prompt
		  :n n))
         (secret (ai-blog--resolve-secret ai-blog-dall-e-api-key))
         (json-response (ai-blog--send-json-request ai-blog-dall-e-url "POST" params (concat "Bearer " secret)))
         (url-list (cdr (assoc 'data json-response))))
    (vconcat (mapcar (lambda (photo)
                       (list
                        (cons 'url (cdr (assoc 'url photo)))
                        (cons 'longdesc "")
                        (cons 'alt (format "Image generated by DALL-E with prompt: %s" prompt))))
                     url-list))))

(defun ai-blog--pexels-request-images (prompt n)
  "Request N images from the pexels API using PROMPT."
  (unless ai-blog-pexels-api-key
    (error "Pexels API key not set"))
  (message "Searching images on Pexels...")
  (let* ((params (list
		  :query prompt
		  :per_page (number-to-string n)
		  :page "1"))
         (secret (ai-blog--resolve-secret ai-blog-pexels-api-key))
	 (json-response (ai-blog--send-json-request ai-blog-pexels-url "GET" params secret))
	 (url-list (cdr (assoc 'photos  json-response))))
    (vconcat (mapcar (lambda (photo)
	               (list
                        (cons 'url (cdr (assoc 'original (cdr (assoc 'src photo)))))
                        (cons 'longdesc (cdr (assoc 'url photo)))
                        (cons 'alt (format "%s by %s on Pexels"
                                           (cdr (assoc 'alt photo))
                                           (cdr (assoc 'photographer photo))))))
                     url-list))))

(assoc 'cu (list (cons 'url "batata") (cons 'cu "porra")))

(defun ai-blog--insert-image-in-menu (&optional url i)
  "Insert an image from URL in the menu buffer with index I."
  (unless url (setq url (url-get-url-at-point)))
  (unless url
    (error "Couldn't find URL"))
  (let ((buffer (url-retrieve-synchronously url)))
    (unwind-protect
        (let ((data (with-current-buffer buffer
                      (goto-char (point-min))
                      (search-forward "\n\n")
                      (buffer-substring (point) (point-max)))))
	  (switch-to-buffer ai-blog-image-buffer-name)
	  (insert (number-to-string i))
	  (insert ".")
          (insert-image (create-image data nil t :scale 0.1))
	  (insert "\n\n\n"))
      (kill-buffer buffer))))

(defun ai-blog--choose-image-from-menu (image-url-list)
  "Choose an image from IMAGE-URL-LIST."
  (let* ((index 1))
    (switch-to-buffer ai-blog-image-buffer-name)
    (erase-buffer)
    (mapc (lambda (image-url)
	    (progn
	      (ai-blog--insert-image-in-menu (cdr (assoc 'url image-url)) index)
	      (setq index (1+ index))))
	  image-url-list)
    (let ((chosen-number (read-number "Choose which image to use (enter a number): ")))
      (kill-buffer ai-blog-image-buffer-name)
      (aref image-url-list (1- chosen-number)))))

(defun ai-blog--dall-e-helper (prompt n)
  "Helper function for generating N images using DALL-E API with prompt PROMPT."
  (let ((images (ai-blog--dall-e-request-images prompt n)))
    (ai-blog--choose-image-from-menu images)))

;;;###autoload
(defun ai-blog-generate-image-dall-e (prompt)
  "Generate an image for a blog post using DALL-E API with PROMPT."
  (interactive)
  (ai-blog--dall-e-helper prompt ai-blog-dall-e-image-count))

(defun ai-blog--pexels-helper (prompt n)
  "Helper function for generating N images using Pexels API with prompt PROMPT."
  (let ((images (ai-blog--pexels-request-images prompt n)))
    (ai-blog--choose-image-from-menu images)))

;;;###autoload
(defun ai-blog-generate-image-pexels (prompt)
  "Generate an image for a blog post using Pexels API with PROMPT."
  (interactive)
  (ai-blog--pexels-helper prompt ai-blog-pexels-image-count))

(defun ai-blog-download-image (url)
  "Download an image from URL."
  (let* ((buf (current-buffer))
         (dir (expand-file-name
	       easy-hugo-image-directory
	       (expand-file-name "static" easy-hugo-basedir)))
         (file (expand-file-name
                (format "%s/%s-%d.png" dir (file-name-base (buffer-file-name)) (random 10000)))))
    (url-copy-file url file)
    file))

(defun ai-blog-insert-image (buf image)
  "Insert an image for a blog post from IMAGE into BUF with CAPTION."
  (interactive)
  (let ((file (ai-blog-download-image (cdr (assoc 'url image)))))
    (switch-to-buffer buf)
    (with-current-buffer buf
      (insert
       (concat
        (format "{{< figure src=\"/%s/%s\" alt=\"%s\" longdesc=\"%s\" >}}"
                easy-hugo-image-directory
                (file-name-nondirectory file)
                (cdr (assoc 'alt image))
                (cdr (assoc 'longdesc image))))))))

(defun ai-blog-insert-featured-image (buf image)
  "Insert an image for a blog post from IMAGE into BUF."
  (interactive)
  (let ((file (ai-blog-download-image (cdr (assoc 'url image)))))
    (switch-to-buffer buf)
    (with-current-buffer buf
      (goto-char (point-min))
      (forward-line 1)
      (recenter-top-bottom)
      (insert
       (format
        "%s: \"%s/%s\""
        ai-blog-featured-image-field-name
        easy-hugo-image-directory
        (file-name-nondirectory file))))))

;;;###autoload
(defun ai-blog-insert-image-dall-e ()
  "Insert a image for a blog post using DALL-E API."
  (interactive)
  (let ((prompt (read-string "Enter a prompt: ")))
    (ai-blog-insert-image
     (current-buffer)
     (ai-blog-generate-image-dall-e prompt))))

;;;###autoload
(defun ai-blog-insert-image-pexels ()
  "Insert a image for a blog post using pexels API."
  (interactive)
  (let ((prompt (read-string "Enter a prompt: ")))
    (ai-blog-insert-image
     (current-buffer)
     (ai-blog-generate-image-pexels prompt))))

;;;###autoload
(defun ai-blog-insert-featured-image-dall-e ()
  "Insert a featured image for a blog post using DALL-E API."
  (interactive)
  (let ((prompt (read-string "Enter a prompt: ")))
    (ai-blog-insert-featured-image (current-buffer) (ai-blog-generate-image-dall-e prompt))))

;;;###autoload
(defun ai-blog-insert-featured-image-pexels ()
  "Insert a featured image for a blog post using pexels API."
  (interactive)
  (let ((prompt (read-string "Enter a prompt: ")))
    (ai-blog-insert-featured-image (current-buffer) (ai-blog-generate-image-pexels prompt))))

(provide 'ai-blog)

;;; ai-blog.el ends here
