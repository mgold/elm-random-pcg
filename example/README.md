# Random.Pcg Examples

These two 0.17 examples show how you can either track a seed to generate random numbers, or use Cmds to generate them.
Both perform the fairly simple task of rolling a six-sided die every second.

Seeds allow random values to be generated synchronously and reproducibly. Commands allows "random numbers as a service"
where values are requested and then you get a response. Commands mean you don't have to thread seeds through your
program.

Or: seeds add a field the to Model, commands add a tag to Msg.

If you need randomness in deeply nested components (not demoed), you can either initialize the components with an
independent seed, or thread their commands up to main (which you're probably doing anyway).

Regardless, you use Generators to describe how to build your random value, and complex recipes should be encoded using
`map` and `andThen` inside the generator.
