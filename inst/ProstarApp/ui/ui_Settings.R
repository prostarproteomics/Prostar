tabPanel(title="Global settings",
         value="GlobalSettingsTab",
         # selectInput("settings_InteractivePlots",
         #             "Type of plots",
         #             choices = c("Interactive (nice but slower)" = "Interactive",
         #                     "Static (faster)" = "Static")),
         numericInput("settings_nDigits", "Numerical precision (digits)", value=3, min=0)
         
         
)
         