import scala.collection.mutable.ArrayBuffer
import scala.math._
import DoubleUtils._

class MathVector
{
    var length: Int = 0
    var array: ArrayBuffer[Double] = new ArrayBuffer[Double]

    def this(length: Int)
    {
        this()
        this.length = length
        for (i <- 0 until this.length)
        {
            this.array.append(0)
        }
    }

    def this(buffer: ArrayBuffer[Double])
    {
        this()
        this.length = buffer.length
        this.array = buffer.map(ele => ele)
    }

    override def toString(): String = 
    {
        this.array.map(_.toString).reduce((a, b) => a + "," + b)
    }

    def getElement(index: Int): Double = 
    {
        assert(index < this.length && index >= 0)
        this.array(index)
    }

    def setElement(index: Int, value: Double): Unit = 
    {
        assert(index >= 0 && index < this.length)
        this.array(index) = value
    }

    def getLength(): Int = this.length

    def toMathVector(buffer: ArrayBuffer[Double]): MathVector = new MathVector(buffer)

    def + (that: MathVector): MathVector = 
    {
        assert(this.length == that.length)
        toMathVector(this.array.zip(that.array).map(element => element._1 + element._2))
    }

    def - (that: MathVector): MathVector = 
    {
        assert(this.length == that.length)
        toMathVector(this.array.zip(that.array).map(ele => -ele._1 + ele._2))
    }

    def * (that: MathVector): Double = 
    {
        assert(this.length == that.length)
        this.array.zip(that.array).map(ele => ele._1*ele._2).reduce(_+_)
    }

    def * (factor: Double): MathVector = this.toMathVector(this.array.map(ele => ele*factor))

    def norm(): Double = sqrt(this*this)
}

object test
{
    def main(args: Array[String]):Unit = 
    {
        val buffer1 = new ArrayBuffer[Double]
        for (i <- 0 until 10)
        {
            buffer1.append(i+1)
        }
        val buffer2 = new ArrayBuffer[Double]
        for (i <- 0 until 10)
        {
            buffer2.append(2*i+1)
        }
        val vector: MathVector = new MathVector(buffer1)
        println(vector.toString)
        val temp: MathVector = new MathVector(buffer2)
        println(temp)
        println(temp + vector)
        println(temp - vector)
        println(temp*vector)
        println(temp.norm)
        println(temp*2)
        println(2*temp)
    }
}
