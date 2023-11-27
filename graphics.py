import cairo

def draw_line(context, x1, y1, x2, y2):
    context.move_to(x1, y1)
    context.line_to(x2, y2)
    context.stroke()