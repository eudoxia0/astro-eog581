import csv
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.animation as animation

def plot_stars(input_path, output_name, route=False):
    x: list[float]    = []
    y: list[float]    = []
    z: list[float]    = []
    labels: list[str] = []

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
    dpi = 600
    fig = plt.figure(figsize=(2,2), dpi=dpi)
    ax = fig.add_subplot(111, projection='3d')

    # Create the scatterplot.
    scatter = ax.scatter(x, y, z, c='b', marker='.', s=1, alpha=0.5)

    # Add labels to each star.
    text = [ax.text(x[i], y[i], z[i] + 0.1, label, fontsize=2) for i, label in enumerate(labels)]

    # Draw the impulses.
    impulses = [ax.plot([x[i], x[i]], [y[i], y[i]], [0, z[i]], ':', c='k', linewidth=0.2) for i in range(len(x))]

    # Are we plotting a route? If so, draw the lines between the stars:
    if route:
        lines = [ax.plot(
            [x[i], x[i+1]],
            [y[i], y[i+1]],
            [z[i], z[i+1]],
            color='r',
            linewidth=0.1
        ) for i in range(len(labels)-1)]
    else:
        lines = []

    # Plot the origin plane.
    GAP = 1
    X, Y = np.meshgrid(
        np.linspace(min(x) - GAP, max(x) + GAP, 10),
        np.linspace(min(y) - GAP, max(y) + GAP, 10)
    )
    Z = np.zeros_like(X)
    origin_plane = ax.plot_wireframe(X, Y, Z, rstride=1, cstride=1, linewidths=0.1)

    # Hide the axes and grid planes.
    plt.axis("off")

    # Define the animation function.
    def animate(i):
        ax.view_init(elev=30, azim=i)

    plt.savefig(output_name + ".png", transparent=True, bbox_inches='tight')

    # Create the animation object.
    anim = animation.FuncAnimation(fig, animate, frames=360, interval=20, blit=False)

    # Save the animation as an MP4 file.
    anim.save(output_name + ".mp4", fps=30, extra_args=['-vcodec', 'libx264'], dpi=dpi)

    print(f"Saved {output_name}")

plot_stars("all-stars.csv", "all-stars")
plot_stars("g581-environs.csv", "g581-environs")
plot_stars("route.csv", "route", route=True)
