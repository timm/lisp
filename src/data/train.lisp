(defun weather ()
        (data
                :name 'weather
                :columns '( outlook temperature humidity windy play)
                :egs
                '(
                        ( sunny 69 70 FALSE )
                        ( sunny 75 70 TRUE )
                        ( overcast 64 65 TRUE )
                        ( rainy 70 96 FALSE )
                        ( sunny 80 90 TRUE )
                        ( sunny 85 85 FALSE )
                        ( sunny 72 95 FALSE )
                        ( rainy 65 70 TRUE )
                        ( rainy 68 80 FALSE )
                        ( rainy 71 91 TRUE )
                        )))
