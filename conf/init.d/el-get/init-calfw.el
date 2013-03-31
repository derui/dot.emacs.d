(require 'calfw)
(require 'calfw-org)
(require 'calfw-ical)

(defun my:open-ical ()
  "open my google calendar at calfw."
  (interactive)
   (cfw:open-ical-calendar
    "https://www.google.com/calendar/ical/derutakayu%40gmail.com/private-1041ddb94f2b4584c633275c34243a91/basic.ics"))
