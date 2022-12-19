import csv
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

def plot_stars(input_path, output_path, route=False):
    # Data.
    x = []
    y = []
    z = []
    labels = []

    # Parse the CSV.
    with open(input_path, newline='') as stream:
        reader = csv.reader(stream, delimiter=",", quotechar="\"")
        for row in reader:
            px, py, pz, label = row
            x.append(float(px))
            y.append(float(py))
            z.append(float(pz))
            labels.append(label)

    # Create a figure and a 3D Axes.
    fig = plt.figure(figsize=(6,2), dpi=600)
    ax = fig.add_subplot(111, projection='3d')

    # Create the scatterplot.
    ax.scatter(x, y, z, c='b', marker='.', s=1, alpha=0.5)

    # Add labels to each star.
    for i, label in enumerate(labels):
        ax.text(x[i], y[i], z[i] + 0.1, label, fontsize=2)

    # Draw the impulses.
    for i in range(len(x)):
        ax.plot([x[i], x[i]], [y[i], y[i]], [0, z[i]], ':', c='k', linewidth=0.2)

    # Are we plotting a route? If so, draw the lines between the stars:
    if route:
        for i, label in enumerate(labels):
            if i < len(labels) - 1:
                ax.plot(
                    [x[i], x[i+1]],
                    [y[i], y[i+1]],
                    [z[i], z[i+1]],
                    color='r',
                    linewidth=0.1
                )

    # Plot the origin plane.
    GAP = 1
    X, Y = np.meshgrid(
        np.linspace(min(x) - GAP, max(x) + GAP, 10),
        np.linspace(min(y) - GAP, max(y) + GAP, 10)
    )
    Z = np.zeros_like(X)
    ax.plot_wireframe(X, Y, Z, rstride=1, cstride=1, linewidths=0.1)

    # Hide the axes and grid planes.
    plt.axis("off")

    # Write the plot
    plt.savefig(output_path, transparent=True, bbox_inches='tight')

plot_stars("all-stars.csv", "all-stars.png")
plot_stars("g581-environs.csv", "g581-environs.png")
plot_stars("route.csv", "route.png", route=True)
