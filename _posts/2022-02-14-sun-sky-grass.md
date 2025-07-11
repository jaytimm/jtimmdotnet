---
layout: post
title: "sun, sky & grass"
date: 2022-02-14
---

> Introduction: A quick look at the sun, the sky and grass.

### Sun and Sky Plot

This example demonstrates a simple illustration of the sun and sky using base R plotting.

    # Set up plot
    plot(
      1, 1, 
      xlim = c(0, 10), ylim = c(0, 10), 
      type = "n", xlab = "", ylab = "", 
      xaxt = "n", yaxt = "n", bty = "n"
    )

    # Draw the sky
    rect(0, 0, 10, 10, col = "skyblue", border = NA)

    # Draw the sun
    symbols(7, 7, circles = 1.5, inches = FALSE, add = TRUE, bg = "yellow")

    # Draw grass
    rect(0, 0, 10, 3, col = "green3", border = NA)

![](/images/sunskyplot-1.png)
