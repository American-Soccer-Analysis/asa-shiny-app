annotate_field <- function(color   = "dimgray",
                           fill    = "white",
                           x_scale = 1,
                           y_scale = 1,
                           x_shift = 0,
                           y_shift = 0,
                           limits  = TRUE,
                           curvature = 0.5) {

    marking_layers <- list(
        # Add pitch outline
        ggplot2::annotate(
            geom = "rect",
            xmin = 0 * x_scale + x_shift,
            xmax = 100 * x_scale + x_shift,
            ymin = 0 * y_scale + y_shift,
            ymax = 100 * y_scale + y_shift,
            color = color,
            fill = fill
        ),
        # Centre circle
        ggplot2::annotation_custom(
            grob = grid::circleGrob(r  = grid::unit(1, "npc"),
                                    gp = grid::gpar(col  = color,
                                                    fill = fill,
                                                    lwd = 1.5)),
            xmin = (50-7) * x_scale + x_shift,
            xmax = (50+7) * x_scale + x_shift,
            ymin = (50-7) * y_scale + y_shift,
            ymax = (50+7) * y_scale + y_shift
        ),
        # Centre spot
        ggplot2::annotate(
            geom = "point",
            x = 50 * x_scale + x_shift,
            y = 50 * y_scale + y_shift,
            color = color,
            fill = fill
        ),
        # Halfway line
        ggplot2::annotate(
            "segment",
            x    = 50 * x_scale + x_shift,
            xend = 50 * x_scale + x_shift,
            y    = 0 * y_scale + y_shift,
            yend = 100 * y_scale + y_shift,
            color = color
        ),
        # Add penalty areas (with penalty spot)
        ggplot2::geom_curve(aes(x = 17 * x_scale + x_shift,
                                y = 62.5 * y_scale + y_shift,
                                xend = 17 * x_scale + x_shift,
                                yend = 37.5 * y_scale + y_shift),
                            curvature = curvature * -1,
                            color = color,
                            lwd = 0.4
        ),
        ggplot2::annotate(geom = "rect",
                          xmin = 83 * x_scale + x_shift,
                          xmax = 100 * x_scale + x_shift,
                          ymin = 21.1 * y_scale + y_shift,
                          ymax = 79.9 * y_scale + y_shift,
                          color = color,
                          fill = fill
        ),
        ggplot2::annotate(  # Penalty spot
            geom = "point",
            x = 88.5 * x_scale + x_shift,
            y = 50 * y_scale + y_shift,
            color = color,
            fill = fill
        ),
        ggplot2::geom_curve(aes(x = 83 * x_scale + x_shift,
                                y = 62.5 * y_scale + y_shift,
                                xend = 83 * x_scale + x_shift,
                                yend = 37.5 * y_scale + y_shift),
                            curvature = curvature,
                            color = color,
                            lwd = 0.4
        ),
        ggplot2::annotate(geom = "rect",
                          xmin = 0 * x_scale + x_shift,
                          xmax = 17 * x_scale + x_shift,
                          ymin = 21.1 * y_scale + y_shift,
                          ymax = 79.9 * y_scale + y_shift,
                          color = color,
                          fill = fill
        ),
        ggplot2::annotate(  # Penalty spot
            geom = "point",
            x = 11.5 * x_scale + x_shift,
            y = 50 * y_scale + y_shift,
            color = color,
            fill = fill
        ),
        # Add 6 yard boxes
        ggplot2::annotate(geom = "rect",
                          xmin = 94.2 * x_scale + x_shift,
                          xmax = 100 * x_scale + x_shift,
                          ymin = 36.8 * y_scale + y_shift,
                          ymax = 63.2 * y_scale + y_shift,
                          color = color,
                          fill = fill
        ),
        ggplot2::annotate(geom = "rect",
                          xmin = 0 * x_scale + x_shift,
                          xmax = 5.8 * x_scale + x_shift,
                          ymin = 36.8 * y_scale + y_shift,
                          ymax = 63.2 * y_scale + y_shift,
                          color = color,
                          fill = fill
        ),
        # Add goals
        ggplot2::annotate(geom = "rect",
                          xmin = 100 * x_scale + x_shift,
                          xmax = 102 * x_scale + x_shift,
                          ymin = 44.2 * y_scale + y_shift,
                          ymax = 55.8 * y_scale + y_shift,
                          color = color,
                          fill = fill
        ),
        ggplot2::annotate(geom = "rect",
                          xmin = 0 * x_scale + x_shift,
                          xmax = -2 * x_scale + x_shift,
                          ymin = 44.2 * y_scale + y_shift,
                          ymax = 55.8 * y_scale + y_shift,
                          color = color,
                          fill = fill
        ),
        # Add corner markings
        ggplot2::geom_curve(aes(x = 0 * x_scale + x_shift,
                                y = 2.75 * y_scale + y_shift,
                                xend = 1.75 * x_scale + x_shift,
                                yend = 0 * y_scale + y_shift),
                            curvature = curvature * -1,
                            color = color,
                            lwd = 0.4
        ),
        ggplot2::geom_curve(aes(x = 0 * x_scale + x_shift,
                                y = 97.25 * y_scale + y_shift,
                                xend = 1.75 * x_scale + x_shift,
                                yend = 100 * y_scale + y_shift),
                            curvature = curvature,
                            color = color,
                            lwd = 0.4
        ),
        ggplot2::geom_curve(aes(x = 98.25 * x_scale + x_shift,
                                y = 0 * y_scale + y_shift,
                                xend = 100 * x_scale + x_shift,
                                yend = 2.75 * y_scale + y_shift),
                            curvature = curvature * -1,
                            color = color,
                            lwd = 0.4
        ),
        ggplot2::geom_curve(aes(x = 98.25 * x_scale + x_shift,
                                y = 100 * y_scale + y_shift,
                                xend = 100 * x_scale + x_shift,
                                yend = 97.25 * y_scale + y_shift),
                                curvature = curvature,
                                color = color,
                                lwd = 0.4
        )
    )

    if (!limits) {
        return(marking_layers)
    }

    limit_layers <- list(
        ggplot2::xlim(-1, 101),
        ggplot2::ylim(-5, 101)  # -5 so that we leave room for direction_label()
    )

    append(
        marking_layers,
        limit_layers
    )
}

# Source: https://github.com/Torvaney/ggsoccer
