(defparameter *real_fm_4*
  '(and eShop
  (and store_front
    (opt homepage
      (or _id_1 (and _id_2
          (and _id_3
            (or _id_5 special_offers))
          (and _id_6
            (or _id_8 _id_9)))))
    (opt registration
      (and registration_enforcement
        (or _id_11 register_to_buy _id_12))
      (and _id_13
        _id_14
        (opt shipping_address
          _id_15)
        (opt _id_16
          _id_17)
        (opt _id_18
          _id_19
          _id_20
          _id_21
          _id_22)
        (opt _id_23
          (or _id_25 _id_26 _id_27 _id_28))
        _id_29
        (opt preferences
          (or _id_31 _id_32 _id_33))
        _id_34
        quick_checkout_profile
        _id_35)
      user_behaviour_tracking_info)
    (and catalog
      (and product_information
        (and product_type
          (or eletronic_goods physical_goods services))
        basic_information
        detailed_information
        warranty_information
        customer_reviews
        (opt associated_assets
          (or _id_38 (and _id_39
              (or (and _id_41
                  (or _id_43 _id_44 _id_45 _id_46 _id_47 _id_48)) _id_49 _id_50))))
        (opt product_variants
          _id_51)
        size
        weight
        availability
        custom_fields)
      (opt categories
        (and categories_catalog
          (opt _id_52
            _id_53
            _id_54)))
      _id_55
      (opt _id_56
        (or _id_58 _id_59))
      (opt _id_60
        _id_61
        category_page
        (opt _id_62
          (opt _id_63
            (or _id_65 _id_66 _id_67 _id_68 _id_69))))
      (opt _id_70
        _id_71
        _id_72))
    (opt wish_list
      wish_list_saved_after_session
      email_wish_list
      _id_73
      (opt permissions
        (or _id_75 _id_76 _id_77)))
    (and buy_paths
      (and _id_78
        _id_79
        _id_80
        _id_81
        _id_82)
      (and _id_83
        (and _id_84
          (or (and registered_checkout
              (opt quick_checkout
                _id_86)) _id_87))
        (opt shipping_options
          _id_88
          _id_89
          _id_90
          _id_91
          _id_92)
        (and _id_93
          (or (and _id_95
              (and _id_96
                (or _id_98 (and _id_99
                    _id_100
                    (and _id_101
                      shipping_2
                      _id_102)
                    (opt _id_103
                      (or _id_105 _id_106 _id_107)))))
              (and _id_108
                (or _id_110 _id_111))) (and _id_112
              (or _id_114 _id_115 _id_116))))
        (and _id_117
          (and _id_118
            (or _id_120 _id_121 _id_122 _id_123 _id_124 _id_125 _id_126 _id_127 _id_128))
          _id_129
          (opt _id_130
            (or _id_132 _id_133 _id_134 _id_135 _id_136 _id_137 _id_138))))
      (and _id_139
        (or _id_141 _id_142 _id_143 _id_144))
      (or (and buy_paths_288_289
          buy_paths_288_289_290
          buy_paths_288_289_291)))
    (opt customer_service
      (or (and _id_146
          _id_147) _id_148 (and _id_149
          (and _id_150
            (or _id_152 _id_153 _id_154))
          _id_155) (and _id_156
          (or _id_158 _id_159))))
    (opt user_behaviour_tracking
      (and _id_160
        (or locally_visited_pages external_referring_pages behaviour_tracked_previous_purchases))))
  (and business_management
    (and _id_162
      (and _id_163
        (or (and physical_goods_fulfillment
            warehouse_management
            (and shipping
              (or (and _id_166
                  (and _id_167
                    _id_168
                    (opt _id_169
                      (or _id_171 _id_172 _id_173 _id_174)))) (and _id_175
                  (or _id_177 _id_178 _id_179 _id_180 _id_181))))) (and eletronic_goods_fulfillment
            _id_182
            _id_183) (and services_fulfillment
            _id_184
            _id_185))))
    (opt _id_186
      (and _id_187
        (or customer_preferences _id_189 _id_190 targeting_criteria_previous_purchases _id_191 wish_list_content previously_visited_pages _id_192 _id_193))
      (and _id_194
        (or (and _id_196
            (and _id_197
              (or _id_199 _id_200))
            (and _id_201
              (or _id_203 (and _id_204
                  _id_205)))
            _id_206
            _id_207) (and discounts
            (and _id_208
              _id_209
              _id_210
              _id_211)
            (and _id_212
              (or _id_214 _id_215 _id_216))
            (and _id_217
              _id_218
              _id_219)
            (and _id_220
              (or _id_222 _id_223))
            _id_224
            _id_225) (and _id_226
            (or _id_228 (and _id_229
                _id_230) (and _id_231
                _id_232)))))
      (and _id_233
        (or _id_235 _id_236 (and _id_237
            personalized_emails
            _id_238)))
      _id_239)
    (opt _id_240
      _id_241
      _id_242)
    (opt inventory_tracking
      _id_243)
    (opt procurement
      (and _id_244
        _id_245
        (opt automatic
          _id_246)))
    (opt reporting_and_analysis
      _id_247
      _id_248
      _id_249)
    (opt _id_250
      (or fulfillment_system _id_252 procurement_system _id_253))
    (and _id_254
      (and _id_255
        _id_256
        _id_257
        _id_258
        _id_259)
      (and _id_260
        _id_261
        _id_262
        _id_263)))))
