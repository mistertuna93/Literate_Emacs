;; [[file:../literate/71-toolsAnsible.org::*Package Setup][Package Setup:1]]
(use-package yaml-mode
  :straight t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(use-package ansible
  :straight t
  :after yaml-mode
  :hook (yaml-mode . ansible))

(use-package ansible-doc
  :straight t
  :after ansible
  :hook (yaml-mode . ansible-doc-mode))

(use-package company-ansible
  :straight t
  :after (company ansible)
  :config
  (add-to-list 'company-backends 'company-ansible))
;; Package Setup:1 ends here

;; [[file:../literate/71-toolsAnsible.org::*Common Variables and Functions][Common Variables and Functions:1]]
(defgroup mistertuna/ansible nil "Custom settings for Ansible development." :group 'tools)
(defcustom mistertuna/ansible-base-path "~/ansible" "Base path for Ansible projects." :type 'string :group 'mistertuna/ansible)
(defcustom mistertuna/kubernetes-base-path "~/kubernetes" "Base path for Kubernetes configurations." :type 'string :group 'mistertuna/ansible)
(defcustom mistertuna/ansible-inventory-path (expand-file-name "inventory" mistertuna/ansible-base-path) "Path to Ansible inventory files." :type 'string :group 'mistertuna/ansible)
(defcustom mistertuna/ansible-roles-path (expand-file-name "roles" mistertuna/ansible-base-path) "Path to Ansible roles." :type 'string :group 'mistertuna/ansible)
(mapc (lambda (dir) (unless (file-exists-p dir) (make-directory dir t)))
      (list mistertuna/ansible-base-path mistertuna/kubernetes-base-path mistertuna/ansible-inventory-path mistertuna/ansible-roles-path))
;; Common Variables and Functions:1 ends here

;; [[file:../literate/71-toolsAnsible.org::*Sealed Secrets Integration][Sealed Secrets Integration:1]]
(defcustom mistertuna/kubeseal-binary "kubeseal" "Path to kubeseal binary." :type 'string :group 'mistertuna/ansible)
(defcustom mistertuna/sealed-secrets-cert nil "Path to Sealed Secrets public certificate." :type '(choice (const nil) string) :group 'mistertuna/ansible)
(defun mistertuna/ensure-kubeseal () (unless (executable-find mistertuna/kubeseal-binary) (error "Kubeseal not found. Please install it first")))
(defun mistertuna/fetch-sealed-secrets-cert (cluster-name namespace)
  "Fetch the public cert from the sealed-secrets controller in CLUSTER-NAME and NAMESPACE."
  (interactive "sCluster name: \nsNamespace (default: kube-system): ")
  (let* ((namespace (or namespace "kube-system"))
         (cert-path (expand-file-name (format "%s-pub-cert.pem" cluster-name) mistertuna/kubernetes-base-path))
         (cmd (format "kubectl get secret -n %s sealed-secrets-key -o yaml | grep tls.crt | cut -d' ' -f4 | base64 -d > %s" namespace cert-path)))
    (when (zerop (shell-command cmd))
      (setq mistertuna/sealed-secrets-cert cert-path)
      (message "Certificate saved to %s" cert-path))))
(defun mistertuna/seal-secret (input-file output-file &optional namespace name)
  "Seal Kubernetes secret from INPUT-FILE to OUTPUT-FILE."
  (interactive
   (list (read-file-name "Input secret file: ")
         (read-file-name "Output sealed secret file: ")
         (read-string "Namespace (default: default): ")
         (read-string "Secret name: ")))
  (mistertuna/ensure-kubeseal)
  (unless mistertuna/sealed-secrets-cert
    (error "No certificate found. Run mistertuna/fetch-sealed-secrets-cert first"))
  (let ((cmd (format "%s --format yaml --cert %s < %s > %s"
                    mistertuna/kubeseal-binary
                    mistertuna/sealed-secrets-cert
                    input-file
                    output-file)))
    (when (and namespace name)
      (setq cmd (format "%s --scope namespace-wide --namespace %s --name %s %s"
                       mistertuna/kubeseal-binary
                       namespace
                       name
                       cmd)))
    (if (zerop (shell-command cmd))
        (message "Secret sealed successfully to %s" output-file)
      (error "Failed to seal secret"))))
(defun mistertuna/create-secret-template ()
  "Create a template for a Kubernetes secret."
  (interactive)
  (let* ((name (read-string "Secret name: "))
         (namespace (read-string "Namespace (default: default): "))
         (type (read-string "Secret type (default: Opaque): "))
         (data-count (read-number "Number of data entries: "))
         (buffer (get-buffer-create "*secret-template*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "apiVersion: v1\nkind: Secret\nmetadata:\n  name: %s\n" name))
      (when (not (string-empty-p namespace))
        (insert (format "  namespace: %s\n" namespace)))
      (insert (format "type: %s\ndata:\n" (if (string-empty-p type) "Opaque" type)))
      (dotimes (i data-count)
        (let ((key (read-string (format "Key %d: " (1+ i))))
              (value (read-string (format "Value %d: " (1+ i)))))
          (insert (format "  %s: %s\n" 
                         key 
                         (base64-encode-string value t)))))
      (yaml-mode)
      (switch-to-buffer buffer))))
;; Sealed Secrets Integration:1 ends here

;; [[file:../literate/71-toolsAnsible.org::*Ansible Vault Integration][Ansible Vault Integration:1]]
(defcustom mistertuna/ansible-vault-path (expand-file-name ".vault" mistertuna/ansible-base-path) "Directory for vault-related files." :type 'string :group 'mistertuna/ansible)
(defvar mistertuna/ansible-vault-passwords (make-hash-table :test 'equal) "Hash table storing vault passwords for different projects.")
(defun mistertuna/ansible-vault-ensure-path () (unless (file-exists-p mistertuna/ansible-vault-path) (make-directory mistertuna/ansible-vault-path t) (set-file-modes mistertuna/ansible-vault-path #o700)))
(defun mistertuna/ansible-vault-get-password-file (project)
  "Get or create vault password file for PROJECT."
  (let ((password-file (expand-file-name (concat project ".vault") mistertuna/ansible-vault-path)))
    (mistertuna/ansible-vault-ensure-path)
    (unless (file-exists-p password-file)
      (with-temp-file password-file
        (set-file-modes password-file #o600)))
    password-file))
(defun mistertuna/ansible-vault-encrypt-string (string &optional project)
  "Encrypt STRING using ansible-vault for PROJECT."
  (interactive "sString to encrypt: \nsProject name (default: default): ")
  (let* ((project (or project "default"))
         (password-file (mistertuna/ansible-vault-get-password-file project))
         (temp-file (make-temp-file "vault-")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert string))
          (shell-command-to-string
           (format "ansible-vault encrypt --vault-password-file=%s %s"
                   password-file temp-file))
          (with-temp-buffer
            (insert-file-contents temp-file)
            (buffer-string)))
      (delete-file temp-file))))
(defun mistertuna/ansible-vault-decrypt-string (string &optional project)
  "Decrypt STRING using ansible-vault for PROJECT."
  (interactive "sString to decrypt: \nsProject name (default: default): ")
  (let* ((project (or project "default"))
         (password-file (mistertuna/ansible-vault-get-password-file project))
         (temp-file (make-temp-file "vault-")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert string))
          (shell-command-to-string
           (format "ansible-vault decrypt --vault-password-file=%s %s"
                   password-file temp-file))
          (with-temp-buffer
            (insert-file-contents temp-file)
            (buffer-string)))
      (delete-file temp-file))))
(defun mistertuna/ansible-vault-edit-file (file &optional project)
  "Edit encrypted FILE using ansible-vault for PROJECT."
  (interactive "fFile to edit: \nsProject name (default: default): ")
  (let* ((project (or project "default"))
         (password-file (mistertuna/ansible-vault-get-password-file project)))
    (find-file file)
    (setq-local ansible-vault-password-file password-file)))
;; Ansible Vault Integration:1 ends here

;; [[file:../literate/71-toolsAnsible.org::*Ansible Pattern Templates][Ansible Pattern Templates:1]]
(defvar mistertuna/ansible-pattern-templates
  '(("role" . ((:name . "Standard Role Structure")
               (:description . "Creates a complete Ansible role structure")
               (:template . (("defaults/main.yml" . "---\n# Default variables\n")
                           ("handlers/main.yml" . "---\n# Handlers\n")
                           ("meta/main.yml" . "---\ngalaxy_info:\n  author: your_name\n")
                           ("tasks/main.yml" . "---\n# Tasks\n")
                           ("vars/main.yml" . "---\n# Role variables\n")))))
    ("playbook" . ((:name . "Basic Playbook")
                   (:description . "Standard playbook with common patterns")
                   (:template . "---\n- name: ${1:Playbook description}\n  hosts: ${2:target}\n  become: ${3:yes}\n  vars:\n    ${4:var_name}: ${5:value}\n  tasks:\n    - name: ${6:Task description}\n      ${7:module}: ${8:options}\n")))
    ("inventory" . ((:name . "Inventory Structure")
                    (:description . "Template for inventory with groups")
                    (:template . "[${1:group_name}]\n${2:host1} ansible_host=${3:ip}\n\n[${1:group_name}:vars]\nansible_user=${4:user}\n")))
    ("kubernetes" . ((:name . "Kubernetes Deployment")
                    (:description . "Template for Kubernetes deployment with Sealed Secrets")
                    (:template . (("inventory/kubernetes.yml" . "---\nall:\n  children:\n    kubernetes:\n      hosts:\n        ${1:cluster_name}:\n          ansible_host: ${2:api_endpoint}\n          ansible_connection: local\n")
                                ("group_vars/kubernetes/main.yml" . "---\nkubernetes_version: ${1:1.27}\nkubeconfig_path: ${2:~/.kube/config}\n")
                                ("roles/kubernetes-setup/tasks/main.yml" . "---\n- name: Ensure kubeconfig exists\n  stat:\n    path: \"{{ kubeconfig_path }}\"\n  register: kubeconfig\n\n- name: Fail if no kubeconfig\n  fail:\n    msg: \"No kubeconfig found at {{ kubeconfig_path }}\"\n  when: not kubeconfig.stat.exists\n")
                                ("roles/kubernetes-setup/defaults/main.yml" . "---\nkubeconfig_path: ~/.kube/config\n")
                                ("secrets/README.md" . "# Secrets Directory\n\nStore your raw secret files here. They will be sealed using kubeseal.\nDO NOT commit raw secrets to git.\n")
                                (".gitignore" . "secrets/*\n!secrets/README.md\n"))))))
  "Collection of Ansible patterns and templates.")
(defun mistertuna/ansible-create-from-pattern (pattern-key directory)
  "Create Ansible files from PATTERN-KEY in DIRECTORY."
  (interactive
   (list (completing-read "Pattern: " (mapcar #'car mistertuna/ansible-pattern-templates))
         (read-directory-name "Directory: ")))
  (let* ((pattern (cdr (assoc pattern-key mistertuna/ansible-pattern-templates)))
         (template (cdr (assoc :template pattern))))
    (if (stringp template)
        ;; Single file template
        (with-temp-buffer
          (insert template)
          (write-file (expand-file-name (format "%s.yml" pattern-key) directory)))
      ;; Directory structure template
      (dolist (file template)
        (let ((file-path (expand-file-name (car file) directory)))
          (make-directory (file-name-directory file-path) t)
          (with-temp-buffer
            (insert (cdr file))
            (write-file file-path)))))))
(defun mistertuna/ansible-insert-pattern (pattern-key)
  "Insert an Ansible pattern from PATTERN-KEY at point."
  (interactive
   (list (completing-read "Pattern: " (mapcar #'car mistertuna/ansible-pattern-templates))))
  (let* ((pattern (cdr (assoc pattern-key mistertuna/ansible-pattern-templates)))
         (template (cdr (assoc :template pattern))))
    (when (stringp template)
      (insert template))))
;; Ansible Pattern Templates:1 ends here

;; [[file:../literate/71-toolsAnsible.org::*Project Management][Project Management:1]]
(defun mistertuna/create-k8s-project (project-name)
  "Create a new Kubernetes project structure."
  (interactive "sProject name: ")
  (let ((project-dir (expand-file-name project-name mistertuna/kubernetes-base-path)))
    (mistertuna/ansible-create-from-pattern "kubernetes" project-dir)
    (message "Created Kubernetes project at %s" project-dir)))
(defun mistertuna/manage-project-secrets (project-name)
  "Manage secrets for a Kubernetes project."
  (interactive 
   (list (completing-read "Project: " 
                         (directory-files mistertuna/kubernetes-base-path nil "^[^.]"))))
  (let* ((project-dir (expand-file-name project-name mistertuna/kubernetes-base-path))
         (secrets-dir (expand-file-name "secrets" project-dir))
         (action (completing-read "Action: " 
                                '("Create new secret" 
                                  "Seal existing secret" 
                                  "View sealed secret"))))
    (pcase action
      ("Create new secret"
       (mistertuna/create-secret-template))
      ("Seal existing secret"
       (let* ((input-file (read-file-name "Secret file: " secrets-dir))
              (output-file (read-file-name "Sealed secret file: " 
                                         (file-name-directory input-file)
                                         nil nil
                                         (concat (file-name-base input-file) 
                                               ".sealed.yaml"))))
         (mistertuna/seal-secret input-file output-file)))
      ("View sealed secret"
       (find-file (read-file-name "Sealed secret file: " secrets-dir))))))
;; Project Management:1 ends here

;; [[file:../literate/71-toolsAnsible.org::*Project-Specific Configurations][Project-Specific Configurations:1]]
(defcustom mistertuna/oracle-cloud-config nil "Path to Oracle Cloud configuration file." :type '(choice (const nil) string) :group 'mistertuna/ansible)
(defun mistertuna/setup-oracle-cloud-project ()
  "Setup a new Oracle Cloud infrastructure project."
  (interactive)
  (let* ((project-name (read-string "Project name: "))
         (project-dir (expand-file-name project-name mistertuna/kubernetes-base-path))
         (config-file (expand-file-name "oracle-cloud.yml" project-dir)))
    ;; Create project structure
    (mistertuna/create-k8s-project project-name)
    ;; Create Oracle Cloud specific configuration
    (with-temp-file config-file
      (insert "---\n")
      (insert "oracle_cloud:\n")
      (insert (format "  project_name: %s\n" project-name))
      (insert "  region: ${1:region}\n")
      (insert "  compartment_id: ${2:compartment_id}\n")
      (insert "  kubernetes_version: ${3:v1.27.2}\n")
      (insert "  node_pools:\n")
      (insert "    - name: ${4:pool1}\n")
      (insert "      shape: ${5:VM.Standard.E4.Flex}\n")
      (insert "      node_count: ${6:3}\n"))
    ;; Fetch Sealed Secrets certificate
    (when (y-or-n-p "Fetch Sealed Secrets certificate from cluster? ")
      (call-interactively #'mistertuna/fetch-sealed-secrets-cert))
    (message "Oracle Cloud project created at %s" project-dir)))
;; Project-Specific Configurations:1 ends here

- ansible-core >= 2.12
- kubectl (for Kubernetes integration)
- kubeseal (for Sealed Secrets)
- base64 (for secret encoding)

;; [[file:../literate/71-toolsAnsible.org::*Key Bindings][Key Bindings:1]]
(use-package general
  :straight t
  :config
  (general-create-definer mistertuna/ansible-leader
    :prefix "C-c a"
    :states '(normal visual emacs))

  (mistertuna/ansible-leader
    ;; Vault operations
    "v" '(:ignore t :which-key "vault")
    "ve" 'mistertuna/ansible-vault-encrypt-file
    "vd" 'mistertuna/ansible-vault-decrypt-file
    "vv" 'mistertuna/ansible-vault-view-file
    
    ;; Sealed Secrets operations
    "s" '(:ignore t :which-key "sealed-secrets")
    "sc" 'mistertuna/create-secret-template
    "ss" 'mistertuna/seal-secret
    "sf" 'mistertuna/fetch-sealed-secrets-cert
    
    ;; Project operations
    "p" '(:ignore t :which-key "project")
    "pk" 'mistertuna/create-k8s-project
    "ps" 'mistertuna/manage-project-secrets
    "po" 'mistertuna/setup-oracle-cloud-project
    
    ;; Pattern operations
    "t" '(:ignore t :which-key "templates")
    "tp" 'mistertuna/ansible-create-from-pattern
    "ti" 'mistertuna/ansible-insert-pattern))
;; Key Bindings:1 ends here
