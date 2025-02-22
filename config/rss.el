(use-package elfeed
  :straight t
  :config
  (setq elfeed-feeds
        '("https://www.nature.com/nature.rss"
          "https://www.science.org/action/showFeed?type=etoc&feed=rss&jc=science"
          "https://pubmed.ncbi.nlm.nih.gov/rss/search/1fOfQYMp1QUMVg9HoG06sLlkYcemijOCBRLY1dFeN0JogJugEm/?limit=100&utm_campaign=pubmed-2&fc=20250117092618" ;; Twist2
          "http://rss.sciencedirect.com/publication/science/00928674"
          "http://elife.elifesciences.org/rss/recent.xml" ;; eLife
          "https://journals.plos.org/plosbiology/feed/atom" ;; PLoS Biology
          "http://www.pnas.org/rss/current.xml" ;; PNAS
          "http://rss.sciencedirect.com/publication/science/22111247"  ;; Cell Reports
          "https://rss.sciencedirect.com/publication/science/19345909" ;; Cell Stem Cell
          "https://rss.sciencedirect.com/publication/science/09550674" ;; Curr Opin Cell Biol
          "https://rss.sciencedirect.com/publication/science/0959437X" ;; Curr Opin Gen Dev
          "https://rss.sciencedirect.com/publication/science/24523100" ;; Curr Opin Sys Biol
          "https://journals.biologists.com/rss/site_1000005/1000005.xml" ;; Development
          "https://rss.sciencedirect.com/publication/science/15345807" ;; Dev Cell
          "http://www.nature.com/ncomms/current_issue/rss" ;; Nat Comm
          "http://feeds.nature.com/ng/rss/current" ;; Nat Gen
          "http://www.nature.com/nmeth/current_issue/rss"
          "https://pubmed.ncbi.nlm.nih.gov/rss/search/14YzNBPGjPa5Oq-1ALt3vUGjAC4ish-IOxqVHLuo9c-4GVCkY4/?limit=50&utm_campaign=pubmed-2&fc=20250117094320" ;; Rudnicki
          "https://pubmed.ncbi.nlm.nih.gov/rss/search/1xCFUMSbAMYdtC8KJFgKAIIAYo-4xPQwAHWYwe_JY77Z1dmEg0/?limit=50&utm_campaign=pubmed-2&fc=20250117093424" ;; Shendure
          "https://pubmed.ncbi.nlm.nih.gov/rss/search/10IsjDYWWF_hcNrfAx1dT3XTb8sIiPJbB2iIh0P8a7fBSSPQPB/?limit=15&utm_campaign=pubmed-2&fc=20250117092818" ;; Freda
          ))
  (setq elfeed-search-title-max-width 120))
(global-set-key (kbd "C-x w") 'elfeed)

(add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)
(add-to-list 'evil-emacs-state-modes 'elfeed-show-mode)
(add-to-list 'evil-emacs-state-modes 'elfeed-entry-mode)

(use-package pocket-reader
  :straight t
  :config
  ;; this is required to stop some weird ghost item bug
  (setq pocket-reader-default-queries '(":unread")))
(require 'pocket-reader)
