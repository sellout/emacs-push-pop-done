;; push-pop-done.el --- Stack-based task management  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar ppd-current-stack ()
  "The current tasks in progress.")

(defvar ppd-deferred-stacks ()
  "Stacks that have been pushed aside for the time being.
Each element of the list is a list of sub-stacks that start from the task at the
same position in ‘ppd-current-stack’.")

(defvar ppd-history ()
  "Completed and abandoned tasks.")

(defun ppd-clear-stacks ()
  (interactive)
  (setf ppd-current-stack ()
        ppd-deferred-stacks ()))

(defun ppd--push-deferred-stacks (stacks)
  "Ensure that empty stacks get discarded."
  (push (cl-remove-if #'null stacks) ppd-deferred-stacks))

(defun ppd-start-new-task (new-task)
  "A task can be anything, but a simple string is the usual thing."
  (interactive "sNew Task: ")
  (push new-task ppd-current-stack)
  (ppd--push-deferred-stacks ())
  (cdr  ppd-current-stack))

(defun ppd-defer-current-task ()
  "Put the current task aside."
  (interactive)
  (let ((task (pop ppd-current-stack))
        (deferred (pop ppd-deferred-stacks)))
    (setf (car ppd-deferred-stacks)
          (append (or (mapcar (lambda (substack) (cons task substack)) deferred)
                      (list (list task)))
                  (car ppd-deferred-stacks)))
    (car ppd-current-stack)))

(defun ppd-deferred-tasks ()
  (car ppd-deferred-stacks))

(cl-defun ppd-resume-deferred-task (&optional index)
  "Grabs a task that starts from the current task and makes that the current one.
If no INDEX is provided, the most recently deferred task is chosen."
  (interactive "nResume task at index: ")
  (let ((index (or index 0))
        (deferred (pop ppd-deferred-stacks)))
    (if (< index (length deferred))
        (let* ((chosen-stack (nth index deferred))
               (task (car chosen-stack))
               (prefixed (cl-remove-if-not (lambda (elem) (eq (car elem) task))
                                           deferred))
               (unprefixed (cl-remove-if (lambda (elem) (eq (car elem) task))
                                         deferred)))
          (ppd--push-deferred-stacks unprefixed)
          (ppd--push-deferred-stacks (mapcar #'cdr prefixed))
          (push task ppd-current-stack))
      (warn "Tried to resume a task that doesn’t exist."
            index
            (length deferred))
      (ppd--push-deferred-stacks deferred))
    (car ppd-current-stack)))

(defun ppd--end-current-task (tag)
  "Move the current task (and it’s deferred stacks) to the history, annotated with TAG."
  (if (car ppd-deferred-stacks)
      (ppd-resume-deferred-task)
    (let ((ended-task (pop ppd-current-stack))
          ;; TODO: What should happen to the deferred tasks when a task is ended?
          (deferred (pop ppd-deferred-stacks)))
      (push (list ended-task tag deferred) ppd-history)
      (ppd-current-task))))

(defun ppd-abandon-current-task ()
  "Give up on the task, and leave it incomplete without finishing."
  (interactive)
  (ppd--end-current-task :abandoned))

(defun ppd-finish-current-task ()
  "Finish the task on top of the stack and return the task that should be resumed,
which is the most-recently deferred task, or the parent task if there is nothing
deferred."
  (interactive)
  (ppd--end-current-task :finished))

(defun ppd-current-task ()
  "Return the current task being worked on."
  (interactive)
  (car ppd-current-stack))

(require 'helm)

(defun ppd-helm ()
  (interactive)
  (helm :sources (list (helm-build-sync-source "current tasks"
                         :candidates ppd-current-stack
                         :fuzzy-match t)
                       (helm-build-sync-source "deferred tasks"
                         :candidates (car ppd-deferred-stacks)
                         :fuzzy-match t)
                       (helm-build-sync-source "inaccessible tasks"
                         :candidates (cdr ppd-deferred-stacks)
                         :fuzzy-match t)
                       (helm-build-sync-source "task history"
                         :candidates ppd-history
                         :fuzzy-match t))
        :buffer "*helm ppd*"))

(define-minor-mode ppd-mode
  "Minor mode for Push, Pop, Done!"
  :global t
  :group 'push-pop-done
  :lighter "↕"
  :keymap
  '(("c" . ppd-clear-stacks)
    ("h" . ppd-helm)
    ("n" . ppd-start-new-task)
    ("a" . ppd-abandon-current-task)
    ("d" . ppd-defer-current-task)
    ("f" . ppd-finish-current-task)
    ("r" . ppd-resume-deferred-task)))

(provide 'push-pop-done)
;;; push-pop-done.el ends here
