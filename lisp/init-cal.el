;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 日历的设置

;; 显示农历
(use-package cal-china-x)
(setq mark-holidays-in-calendar t)
(setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
(setq calendar-holidays cal-china-x-important-holidays)

(setq christian-holidays nil) ;; 不显示基督教的节日
(setq hebrew-holidays nil)    ;; 不显示希伯来人的节日
(setq islamic-holidays nil)   ;; 不显示伊斯兰教的节日

(provide 'init-cal)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
