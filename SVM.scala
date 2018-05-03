import scala.collection.mutable.ArrayBuffer
import java.io.{File, PrintWriter}
import scala.io.Source
import sys.process._
import scala.language.postfixOps
import scala.math._
import java.nio.file.{Paths, Files}
import scala.util.Random
import util.control.Breaks._

class SVM
{
    var numberOfFeatures: Int = 0
    var numberOfSamples: Int = 0
    var X: ArrayBuffer[ArrayBuffer[Double]] = new ArrayBuffer[ArrayBuffer[Double]]
    var y: ArrayBuffer[Int] = new ArrayBuffer[Int]
    var alpha: ArrayBuffer[Double] = new ArrayBuffer[Double]
    var C: Double = 0
    var beta: ArrayBuffer[Double] = new ArrayBuffer[Double]
    var beta_0: Double = 0
    var pairs: ArrayBuffer[(Int, Int)] = new ArrayBuffer[(Int, Int)]
    
    def getLines(inputFileName: String): (String, ArrayBuffer[String]) = 
    {
        assert(Files.exists(Paths.get(inputFileName)))
        var counter: Int = 0
        var header: String = ""
        val lines: ArrayBuffer[String] = new ArrayBuffer[String]
        for (line <- Source.fromFile(inputFileName).getLines)
        {
            counter match
            {
                case 0 => 
                {
                    header = line.stripLineEnd
                }
                case _ => 
                {
                    lines.append(line.stripLineEnd)
                }
            }
            counter += 1
        }
        (header, lines)
    }

    def readFile(inputFileName: String): (ArrayBuffer[ArrayBuffer[Double]], ArrayBuffer[Int]) = 
    {
        var (header:String, lines:ArrayBuffer[String]) = this.getLines(inputFileName)
        val data: ArrayBuffer[ArrayBuffer[Double]] = lines.map(line => line.split(",").dropRight(1).map(ele => ele.toDouble).to[ArrayBuffer])
        val label: ArrayBuffer[Int] = lines.map(line => line.split(",").last.toInt)
        return (data, label)
    }

    def getPairs(n: Int): ArrayBuffer[(Int, Int)] = 
    {
        val pairs: ArrayBuffer[(Int, Int)] = new ArrayBuffer[(Int, Int)]
        for (i <- 0 until n)
        {
            for (j <- i + 1 until n)
            {
                pairs.append((i, j))
            }
        }
        pairs
    }

    def getElementPairs[T: Numeric](array: ArrayBuffer[T]): ArrayBuffer[(T, T)] = 
    {
        val pairs: ArrayBuffer[(T, T)] = new ArrayBuffer[(T, T)]
        for (i <- array.indices)
        {
            for (j <- i + 1 until array.length)
            {
                pairs.append((array(i), array(j)))
            }
        }
        pairs
    }

    def shuffle[T <: Any](array: ArrayBuffer[(T, T)]): ArrayBuffer[(T, T)] = 
    {
        val random = new Random
        val limit = array.length
        for (i <- array.indices)
        {
            val random_index = random.nextInt(limit)
            val temp = array(i)
            array(i) = array(random_index)
            array(random_index) = temp
        }
        array
    }

    def this(inputFileName: String, C: Double)
    {
        this()
        assert(C > 0)
        var (header, lines) = getLines(inputFileName)
        this.numberOfFeatures = header.split(",").length - 1
        this.numberOfSamples = lines.length
        this.X = lines.map(line => line.split(",").dropRight(1).map(ele => ele.toDouble).to[ArrayBuffer])
        this.y = lines.map(line => line.split(",").last.toInt)
        for (i <- 0 until this.numberOfSamples)
        {
            this.alpha.append(0)
        }
        for (i <- 0 until this.numberOfFeatures)
        {
            this.beta.append(0)
        }
        this.beta_0 = 0
        this.pairs = this.shuffle(this.getPairs(this.numberOfSamples))
        //this.pairs.foreach(p => println(p._1 + ", " + p._2))
        this.C = C
    }

    def getBeta():Unit = 
    {
        val temp:ArrayBuffer[Double] = this.alpha.zip(this.y).map(ele => ele._1*ele._2)
        this.beta = temp.zip(this.X).map(ele => {
            val alpha_y:Double = ele._1
            val vector:ArrayBuffer[Double] = ele._2
            vector.map(element => element*alpha_y)
        }).reduce((a, b) => SVM.sum(a, b))
        /*
        for (i <- this.beta.indices)
        {
            var s: Double = 0
            for (j <- this.alpha.indices)
            {
                s += this.alpha(j)*this.y(j)*this.X(j)(i)
            }
            this.beta(i) = s
        }*/
    }

    def onBoundary(alpha: Double): Boolean = 
    {
        val eps = 1.0e-10
        abs(alpha) > eps && abs(alpha - this.C) > eps
    }

    def getBeta_0(): ArrayBuffer[Double] = 
    {
        val boundaryIndices = this.alpha.indices.filter(index => this.onBoundary(this.alpha(index))).to[ArrayBuffer]
        val beta_0_values:ArrayBuffer[Double] = boundaryIndices.map(index => this.y(index) - SVM.innerProduct(this.beta, this.X(index)))
        if (beta_0_values.length == 0) return beta_0_values
        this.beta_0 = SVM.mean(beta_0_values)
        return beta_0_values
    }

    def get_alpha_2_limit(alpha_1: Double, alpha_2: Double, y_1: Int, y_2: Int):(Double, Double) = 
    {
        val s = y_1*y_2
        assert(s == 1 || s == -1)
        if (s == -1)
        {
            val k = alpha_1 - alpha_2
            return (max(-k, 0), this.C + min(-k, 0))
        }
        else
        {
            val k = alpha_1 + alpha_2
            return (max(0, k-this.C), min(this.C, k))
        }
    }

    def jointOptimize(alpha_1: Double, alpha_2: Double, x_1: ArrayBuffer[Double], x_2:ArrayBuffer[Double], y_1: Int, y_2: Int, norm: Double): (Double, Double) = 
    {
        val eta: Double = norm*norm
        val alpha_2_star: Double = alpha_2 + y_2*(SVM.innerProduct(this.beta, x_1) - y_1 - (SVM.innerProduct(this.beta, x_2) - y_2))/eta
        val (left, right) = this.get_alpha_2_limit(alpha_1, alpha_2, y_1, y_2)
        val alpha_2_new = SVM.clip(alpha_2_star, left, right)
        val alpha_1_new = alpha_1 - y_1*y_2*(alpha_2_new - alpha_2)
        (alpha_1_new, alpha_2_new)
    }

    def updateEntire():Unit = 
    {
        val eps = 1.0e-10
        val eraNumber = 10
        val sweepTimes = this.pairs.length
        val interval = sweepTimes/eraNumber
        this.getBeta
        for (i <- this.pairs.indices)
        {
            if (interval == 0)
            {
                println("update entire step index = " + i + ", total = " + sweepTimes)
            }
            else
            {
                if ((i+1)%interval == 0)
                {
                    println("update entire step index = " + (i+1)/interval + ", total = " + eraNumber)
                }
            }
            val pair:(Int, Int) = this.pairs(i)
            val first_index: Int = pair._1
            val second_index: Int = pair._2
            val alpha_1 = this.alpha(first_index)
            val alpha_2 = this.alpha(second_index)
            val x_1: ArrayBuffer[Double] = this.X(first_index)
            val x_2: ArrayBuffer[Double] = this.X(second_index)
            val y_1: Int = this.y(first_index)
            val y_2: Int = this.y(second_index)
            val diff = SVM.norm(SVM.subtract(x_1, x_2))
            breakable
            {
                if (diff < eps) break
                else
                {
                    val(alpha_1_new, alpha_2_new) = this.jointOptimize(alpha_1, alpha_2, x_1, x_2, y_1, y_2, diff)
                    this.alpha(first_index) = alpha_1_new
                    this.alpha(second_index) = alpha_2_new
                    val delta_alpha_1 = alpha_1_new - alpha_1
                    val delta_alpha_2 = alpha_2_new - alpha_2
                    this.beta = SVM.sum(this.beta, SVM.sum(x_1.map(ele => ele*y_1*delta_alpha_1), x_2.map(ele => ele*y_2*delta_alpha_2)))
                    /*for (i <- this.beta.indices)
                    {
                        this.beta(i) = this.beta(i) + delta_alpha_1*y_1*x_1(i) + delta_alpha_2*y_2*x_2(i)
                    }*/
                }
            }
        }
    }

    def hasViolatedKKT(alpha: Double, x: ArrayBuffer[Double], y: Int): Boolean = 
    {
        val eps = 1.0e-10
        //assert(abs(alpha) > eps && (this.C - alpha) > eps)
        val value: Double = (SVM.innerProduct(this.beta, x) + this.beta_0)*y
        if (abs(alpha) < eps)
        {
            return !(value >= 1)
        }
        else if (abs(alpha - this.C) < eps)
        {
            return !(value <= 1)
        }
        else 
        {
            return !(abs(value - 1) < eps)
        }
    }

    def fastUpdate(): Boolean = 
    {
        val eps = 1.0e-10
        this.getBeta
        this.getBeta_0
        val possibleIndices:ArrayBuffer[Int] = this.alpha.indices.filter(index => hasViolatedKKT(this.alpha(index), this.X(index), this.y(index))).to[ArrayBuffer]
        println("Number of indices that have violated the KKT condition is " + possibleIndices.length)
        if(possibleIndices.length < 2) return false
        val nonBoundaryIndices:ArrayBuffer[Int] = possibleIndices.filter(index => !this.onBoundary(this.alpha(index)))
        println("Number of non boundary indices that have violated the KKT condition is " + nonBoundaryIndices.length)
        val pairs = this.shuffle(this.getElementPairs(possibleIndices))
        for (i <- pairs.indices)
        {
            val pair = pairs(i)
            val first_index = pair._1
            val second_index = pair._2
            val alpha_1 = this.alpha(first_index)
            val alpha_2 = this.alpha(second_index)
            val x_1: ArrayBuffer[Double] = this.X(first_index)
            val x_2: ArrayBuffer[Double] = this.X(second_index)
            val y_1: Int = this.y(first_index)
            val y_2: Int = this.y(second_index)
            val diff = SVM.norm(SVM.subtract(x_1, x_2))
            breakable
            {
                if (diff < eps) break
                else
                {
                    val (alpha_1_new, alpha_2_new) = this.jointOptimize(alpha_1, alpha_2, x_1, x_2, y_1, y_2, diff)
                    this.alpha(first_index) = alpha_1_new
                    this.alpha(second_index) = alpha_2_new
                    val delta_alpha_1 = alpha_1_new - alpha_1
                    val delta_alpha_2 = alpha_2_new - alpha_2
                    this.beta = SVM.sum(this.beta, SVM.sum(x_1.map(ele => ele*y_1*delta_alpha_1), x_2.map(ele => ele*y_2*delta_alpha_2)))
                    /*for (i <- this.beta.indices)
                    {
                        this.beta(i) = this.beta(i) + delta_alpha_1*y_1*x_1(i) + delta_alpha_2*y_2*x_2(i)
                    }*/
                }
            }
        }
        return true
    }

    def train(): Unit = 
    {
        val iterationMax: Int = 10
        var counter: Int = 0
        val updateEntireFrequency: Double = 0.5
        val updateEntireInterval: Int = (1.0/updateEntireFrequency).toInt
        val eps: Double = 1.0e-10
        val writer = new PrintWriter(new File("alpha_records.txt"))
        breakable
        {
            while(counter < iterationMax)
            {
                val alphaOld: ArrayBuffer[Double] = this.alpha.map(ele => ele)
                if (counter%updateEntireInterval == 0)
                {
                    this.updateEntire
                }
                else
                {
                    var tempCounter: Int = 0
                    val tempCounterMax: Int = 20
                    breakable
                    {
                        while(this.fastUpdate)
                        {
                            tempCounter += 1
                            if (tempCounter >= tempCounterMax) break
                        }
                    }
                }
                val alphaNew: ArrayBuffer[Double] = this.alpha.map(ele => ele)
                val error: Double = SVM.norm(SVM.subtract(alphaNew, alphaOld))
                println("Counter = " + counter + ", total = " + iterationMax + ", error = " + error)
                writer.write("alpha:" + SVM.arrayBufferToString(this.alpha) + "\n")
                if (error < eps) break
                counter += 1
            }
        }
        writer.close()
        this.getBeta
        val alpha_dot_y = SVM.innerProduct(this.alpha, this.y.map(_.toDouble))
        assert(abs(alpha_dot_y) < eps)
        val parameterFileName = "final_parameters.txt"
        val finalWriter = new PrintWriter(new File(parameterFileName))
        val beta_0_values: ArrayBuffer[Double] = this.getBeta_0
        finalWriter.write("alpha:" + SVM.arrayBufferToString(this.alpha) + "\n")
        finalWriter.write("beta:" + SVM.arrayBufferToString(this.beta) + "\n")
        finalWriter.write("beta_0_values:" + SVM.arrayBufferToString(beta_0_values) + "\n")
        finalWriter.write("beta_0:" + this.beta_0.toString + "\n")
        finalWriter.write("alpha_dot_y:" + alpha_dot_y.toString + "\n")
        finalWriter.close()
    }

    def test(testFileName: String): Unit = 
    {
        assert(Files.exists(Paths.get(testFileName)))
        val (data, labels) = this.readFile(testFileName)
        val predictions: ArrayBuffer[Double] = data.map(vector => SVM.innerProduct(this.beta, vector) + this.beta_0)
        SVM.printFile(predictions, labels, "prediction,label", "prediction_label.csv")
        val confusionMatrix:ArrayBuffer[ArrayBuffer[Double]] = SVM.getConfusionMatrix(predictions, labels, "confusion_matrix.txt")
        assert(confusionMatrix.length == 2)
        assert(confusionMatrix(0).length == 2)
    }
}

object SVM
{
    def arrayToString(array: Array[Double]): String = array.map(ele => ele.toString).reduce((a, b) => a + "," + b)
    def sum(a: Array[Double], b: Array[Double]): Array[Double] = a.zip(b).map(pair => pair._1 + pair._2)
    def sum(a: ArrayBuffer[Double], b: ArrayBuffer[Double]): ArrayBuffer[Double] = a.zip(b).map(pair => pair._1 + pair._2)
    def innerProduct(array_1: ArrayBuffer[Double], array_2:ArrayBuffer[Double]):Double = 
    {
        assert(array_1.length == array_2.length)
        array_1.zip(array_2).map(p => p._1*p._2).reduce(_+_)
    }
    /*def innerProduct[T: Numeric](a: ArrayBuffer[T], b: ArrayBuffer[T]): T = 
    {
        import Numeric.Implicits._
        assert(a.length == b.length)
        var result: T = implicitly[Numeric[T]].zero
        for (i <- a.indices)
        {
            result += a(i)*b(i)
        }
        result
    }*/
    def norm(array: ArrayBuffer[Double]): Double = sqrt(innerProduct(array, array))
    def add(array_1: ArrayBuffer[Double], array_2:ArrayBuffer[Double]): ArrayBuffer[Double] = 
    {
        assert(array_1.length == array_2.length)
        array_1.zip(array_2).map(p => p._1 + p._2)
    }
    def subtract(array_1: ArrayBuffer[Double], array_2: ArrayBuffer[Double]): ArrayBuffer[Double] = 
    {
        assert(array_1.length == array_2.length)
        array_1.zip(array_2).map(p => p._1 - p._2)
    }
    def mean(array:ArrayBuffer[Double]): Double = 
    {
        assert(array.length > 0)
        array.reduce(_+_)/array.length.toDouble
    }
    def mean(array:Array[Double]): Double = 
    {
        assert(array.length > 0)
        array.reduce(_+_)/array.length.toDouble
    }
    def clip(alpha: Double, left: Double, right: Double): Double = 
    {
        //assert(left <= right)
        if (alpha >= left && alpha <= right) return alpha
        else if (alpha < left) return left
        else return right
    }
    def arrayBufferToString[T: Numeric](array:ArrayBuffer[T]): String = array.map(ele => ele.toString).reduce((a, b) => a + "," + b)
    def printFile(x: ArrayBuffer[Double], y: ArrayBuffer[Int], header: String, outputFileName: String): Unit = 
    {
        assert(x.length == y.length)
        val writer = new PrintWriter(outputFileName)
        if (header != "") writer.write(header + "\n")
        for (i <- x.indices)
        {
            writer.write(x(i).toString + "," + y(i).toString + "\n")
        }
        writer.close()
    }
    def getConfusionMatrix(predictions: ArrayBuffer[Double], labels: ArrayBuffer[Int], outputFileName: String): ArrayBuffer[ArrayBuffer[Double]] = 
    {
        assert(predictions.length == labels.length)
        var trueNegative: Double = 0
        var falseNegative: Double = 0
        var falsePositive: Double = 0
        var truePositive: Double = 0
        for (i <- predictions.indices)
        {
            if (predictions(i) > 0)
            {
                if (labels(i) > 0)
                    truePositive += 1
                else
                    falsePositive += 1
            }
            else
            {
                if (labels(i) > 0)
                    falseNegative += 1
                else
                    trueNegative += 1
            }
        }
        val result = new ArrayBuffer[ArrayBuffer[Double]]
        val firstRow: ArrayBuffer[Double] = Array[Double](trueNegative, falseNegative).to[ArrayBuffer]
        val secondRow: ArrayBuffer[Double] = Array[Double](falsePositive, truePositive).to[ArrayBuffer]
        result.append(firstRow)
        result.append(secondRow)
        val eps = 1.0e-10
        val captureRate = (truePositive + eps)/(truePositive + falseNegative + eps)
        val incorrectSlayRate = (falsePositive + eps)/(falsePositive + truePositive + eps)
        println("Confusion matrix: ")
        println(trueNegative + "  " + falseNegative)
        println(falsePositive + "  " + truePositive)
        println("Capture rate = " + captureRate)
        println("Incorrect slay rate = " + incorrectSlayRate)
        val writer = new PrintWriter(new File(outputFileName))
        writer.write("Confusion matrix:\n")
        writer.write(trueNegative.toString + "  " + falseNegative.toString + "\n")
        writer.write(falsePositive.toString + "  " + truePositive.toString + "\n")
        writer.write("Capture rate = " + captureRate.toString + "\n")
        writer.write("Incorrect slay rate = " + incorrectSlayRate.toString + "\n")
        result
    }
}

object svm_test
{
    def split(inputFileName: String): (String, String) = 
    {
        val trainFileName = "train.csv"
        val testFileName = "test.csv"
        readFile
    }
    def main(args: Array[String]): Unit = 
    {
        if (args.length != 2)
        {
            println("inputFileName = args(0), trainRatio = args(1). ")
            System.exit(-1)
        }
        val inputFileName = args(0)
        val trainRatio = args(1).toDouble
        assert(trainRatio > 0 && trainRatio < 1)
        val C = 10000
        val svm = new SVM(trainFileName, C)
        svm.train()
        svm.test(testFileName)
    }
}
