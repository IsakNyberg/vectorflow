# [Vectorflow](https://nyberg.dev/vectorflow/)

Vectorflow is a tool designed for visualizing vector fields using a 2D field equation. It represents the velocity of each point in the field with a particle that traces a path. This project and guide draw inspiration from Anvaka's [Fieldplay](https://github.com/anvaka/fieldplay) repository. The entire project is written in Rust and compiled to Web Assembly. Try it out on [nyberg.dev/vectorflow/](https://nyberg.dev/vectorflow/)

[![field](https://github.com/anvaka/fieldplay/wiki/images/field_1.png)](https://anvaka.github.io/fieldplay/?dt=0.007&fo=0.998&dp=0.009&cm=1&cx=-1.275949999999999&cy=-1.6277&w=30.2937&h=30.2937&code=v.x%20%3D%20length%28p%29*min%28sin%28p.y%29%2Ccos%28p.x%29%29%3B%0Av.y%20%3D%20cos%28%28p.y%2Bp.y%29%29%3B%0A%20%20)
[![field 2](https://github.com/anvaka/fieldplay/wiki/images/field_2.png)](https://anvaka.github.io/fieldplay/?dt=0.007&fo=0.998&dp=0.009&cm=1&cx=-1.275949999999999&cy=-1.62765&w=30.2937&h=30.2937&code=v.x%20%3D%20cos%28p.y%29%3B%0Av.y%20%3D%20cos%28p.x%29%3B%0A%20%20)
[![field 3](https://github.com/anvaka/fieldplay/wiki/images/field_3.png)](https://anvaka.github.io/fieldplay/?dt=0.02&fo=0.998&dp=0.009&cm=1&cx=0.21419999999999995&cy=-0.7710999999999997&w=55.970200000000006&h=55.970200000000006&code=v.x%20%3D%20min%28sin%28exp%28p.x%29%29%2Csin%28length%28p%29%29%29%3B%0Av.y%20%3D%20sin%28p.x%29%3B%0A%20%20)
[![field 4](https://github.com/anvaka/fieldplay/wiki/images/field_4.png)](https://anvaka.github.io/fieldplay/?dt=0.02&fo=0.998&dp=0.009&cm=1&cx=2.43185&cy=-1.1695&w=11.4385&h=11.4385&code=v.x%20%3D%20%28p.y%2Bcos%28p.y%29%29%3B%0Av.y%20%3D%20sin%28min%28length%28p%29%2Clog%28%28p.y%2Bp.x%29%29*p.x%29%29%3B%0A%20%20)


## What?

Let's assign to every point on a grid a vector `(1, 0)`. This means
we have an arrow, pointing to the right:

![Vector field V(1, 0)](https://github.com/anvaka/fieldplay/wiki/images/field_1_0.png)

Let's pretend these vectors represent velocity. What if we drop a thousand particles onto
this grid? How would they move?

![Moving particles in V(1, 0)](https://github.com/anvaka/fieldplay/wiki/images/field_1_0_move.gif)

When we assigned a vector to each point on the plain, we created a mathematical structure
called `Vector Field`.

Let's create a bit more interesting vector field:

* Points with even `y` coordinate get vector `(1, 0)`;
* Points with odd `y` coordinate get an opposite vector `(-1, 0)`;

![Even odd directions](https://github.com/anvaka/fieldplay/wiki/images/field_even_odd.png)

Again we drop a few thousands particles and see what happens:

![Moving even odd directions](https://github.com/anvaka/fieldplay/wiki/images/field_even_odd_move.gif)

The field above can be written in a single formula:

```
(
50.0 * mod(floor(y/100), 2.0) - 25.0
,
0.0
)
```
The floor function effectively rounds down all `y` values to the nearest integer. As a result, the first 100 pixels assume the value `0`, the subsequent 100 pixels take on the value `1`, and so forth. By applying the modulo operation `2` to these values, we obtain `0` for even numbers and `1` for odd numbers. We then manipulate this remainder to generate a final vector, which is either `(-25, 0)` or `(25, 0)`.

Up until now, we've exclusively utilized one component of the velocity vector, specifically `x`, causing the particles to move solely horizontally. Let's experiment with configuring both components and observe the outcomes.

```
(
50.0 * mod(floor(y/100), 2.0) - 25.0
,
50.0 * mod(floor(x/100), 2.0) - 25.0
)
```

![Field x, y](https://github.com/anvaka/fieldplay/wiki/images/field_xy.png)
![Animated field x, y](https://github.com/anvaka/fieldplay/wiki/images/field_xy_small.gif)

Wow! Two simple operations, and the final animation looks like an art piece!

![Field x, y](https://github.com/anvaka/fieldplay/wiki/images/field_xy_final.png)

Vector fields turns out to be very flexible generative framework.

# Parser Syntax
The parser function utilizes the `x` and `y` coordinates of each pixel to compute the corresponding vector. Furthermore, it offers the following functionalities:

```
constants:
pi
e

Single variable functions:
sin(x)
cos(x)
tan(x)
sqrt(x)
abs(x)
floor(x)
ceil(x)

Two variable functions:
mod(x, y)
len(x, y)
```

# Cool Examples
Here are some cool example, feel free to suggest some yourself!

Dipole

```
(
x*y/1000
,
(y*y - x^2)/1000
)
```
Swirls
```
(
x+len(x,y) * sin(sqrt(len(x,y)))
,
y+len(x,y) * cos(sqrt(len(x,x)))
)
```

Black Hole
```
(
y/(len(x,y)^2/100000 - 0.001*x)
,
-x/(len(x,y)^2/10000 - 0.001*y)
)
```
Mosaic
```
(
200*sin(y/100 + x/1000)
,
200*cos(x/100 - y/1000)
)
```
Eye
```
(
x*cos(400/len(x,y)) - y*sin(400/len(x,y))
,
x*sin(400/len(x,y)) + y*cos(400/len(x,y))
)
```
