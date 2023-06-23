# logo creation

# https://github.com/GuangchuangYu/hexSticker
# https://www.reshot.com/free-svg-icons/item/connected-profiles-WZ5SHDYNG8/

library(hexSticker)
library(showtext)

font_add_google('Caveat')
showtext_auto()

sticker('C:/Users/ra93kuy/Documents/GitHub/tidyabm/tools/logo/profiles_raw.png',
        #system.file('tools/logo/profiles_raw.png',
        #            package = 'tidyabm'),
        package = 'tidyABM',       # text to print
        p_family = 'Caveat',       # font
        p_size = 10,               # font size
        p_fontface = 'bold',       # font face
        s_x = 1.085,               # x position for image
        s_y = .78,                 # y position for image
        s_width = .8,              # image width
        h_fill = '#26c6da',        # background color
        h_color = '#177682',       # border color
        filename = 'tools/logo/tidyabm_not-cropped.svg'   # output file
)
