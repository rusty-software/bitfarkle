(ns bitfarkle.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [bitfarkle.game-test]))

(doo-tests 'bitfarkle.game-test)
