object DoubleUtils
{
    implicit class DoubleImprovements(factor: Double)
    {
        def * (vector: MathVector) = vector*factor
    }
}
