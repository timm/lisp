(defun contact-lens ()
  (data
   :name 'contact-lens
   :columns '(age perscription astigmatism tear-production lens)
   :egs
   '(
     (young          myope        yes normal hard)
     (young          hypermetrope yes normal hard)    
     (presbyopic     myope        yes normal hard)
     (pre-presbyopic myope        yes normal hard)
   
     (young          hypermetrope no  reduced none)
     (young          hypermetrope yes reduced none)
     (pre-presbyopic hypermetrope yes reduced none)
     (pre-presbyopic hypermetrope yes normal  none)
     (young          myope        no  reduced none)
     (young          myope        yes reduced none)
     (presbyopic     myope        no  reduced none)
     (presbyopic     myope        no  normal  none)
     (presbyopic     hypermetrope yes reduced none)
     (presbyopic     hypermetrope yes normal  none)
     (presbyopic     myope        yes reduced none)
     (pre-presbyopic hypermetrope no  reduced none)
     (pre-presbyopic myope        no  reduced none)
     (pre-presbyopic myope        yes reduced none)
     (presbyopic     hypermetrope no  reduced none)
     
     (pre-presbyopic myope        no normal soft)
     (pre-presbyopic hypermetrope no normal soft)
     (young          myope        no normal soft)
     (young          hypermetrope no normal soft)
     (presbyopic     hypermetrope no normal soft)
     )))
