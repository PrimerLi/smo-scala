/*
CopyRight 2018
@author Enzhi Li
All rights reserved. 
*/

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
    var C: Double = 0.0
    var CNegative: Double = 0.0
    var CPositive: Double = 0.0
    var beta: ArrayBuffer[Double] = new ArrayBuffer[Double]
    var beta_0: Double = 0
    var pairs: ArrayBuffer[(Int, Int)] = new ArrayBuffer[(Int, Int)]
    val positiveIndices: ArrayBuffer[Int] = new ArrayBuffer[Int]
    val negativeIndices: ArrayBuffer[Int] = new ArrayBuffer[Int]

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

    def this(inputFileName: String, C: Double, positiveFactor: Double)
    {
        this()
        assert(C > 0)
        assert(positiveFactor >= 1.0)
        var (header, lines) = SVM.getLines(inputFileName)
        this.numberOfFeatures = header.split(",").length - 1
        this.numberOfSamples = lines.length
        //this.X = lines.map(line => line.split(",").dropRight(1).map(ele => ele.toDouble).to[ArrayBuffer])
        //this.y = lines.map(line => line.split(",").last.toInt)
        for (i <- lines.indices)
        {
            val tempArray = lines(i).split(",")
            this.X.append(tempArray.dropRight(1).map(ele => ele.toDouble).to[ArrayBuffer])
            val label: Int = tempArray.last.toInt
            this.y.append(label)
            if (label > 0)
            {
                this.positiveIndices.append(i)
            }
            else
            {
                this.negativeIndices.append(i)
            }
        }
        for (i <- 0 until this.numberOfSamples)
        {
            this.alpha.append(0)
        }
        for (i <- 0 until this.numberOfFeatures)
        {
            this.beta.append(0)
        }
        this.beta_0 = 0
        //this.pairs = this.shuffle(this.getPairs(this.numberOfSamples))
        //this.pairs.foreach(p => println(p._1 + ", " + p._2))
        this.C = C
        this.CNegative = C
        this.CPositive = C*positiveFactor
    }

    def this(inputFileName: String, C: Double)
    {
        this(inputFileName, C, 1.0)
    }

    def getBeta():Unit = 
    {
        /*val temp:ArrayBuffer[Double] = this.alpha.zip(this.y).map(ele => ele._1*ele._2)
        this.beta = temp.zip(this.X).map(ele => {
            val alpha_y:Double = ele._1
            val vector:ArrayBuffer[Double] = ele._2
            vector.map(element => element*alpha_y)
        }).reduce((a, b) => SVM.sum(a, b))*/
        for (i <- this.beta.indices)
        {
            var s: Double = 0
            for (j <- this.alpha.indices)
            {
                s += this.alpha(j)*this.y(j)*this.X(j)(i)
            }
            this.beta(i) = s
        }
    }

    def onBoundary(alpha: Double): Boolean = 
    {
        val eps = 1.0e-10
        abs(alpha) > eps && abs(alpha - this.C) > eps
    }

    def onBoundary(alpha: Double, y: Int): Boolean = 
    {
        val eps = 1.0e-10
        y > 0 match
        {
            case true => abs(alpha) > eps && abs(alpha - this.CPositive) > eps
            case _ => abs(alpha) > eps && abs(alpha - this.CNegative) > eps
        }
        /*if (y > 0)
        {
            abs(alpha) > eps && abs(alpha - this.CPositive) > eps
        }
        else
        {
            abs(alpha) > eps && abs(alpha - this.CNegative) > eps
        }*/
    }

    def getBeta_0(): ArrayBuffer[Double] = 
    {
        val boundaryIndices = this.alpha.indices.filter(index => this.onBoundary(this.alpha(index), this.y(index))).to[ArrayBuffer]
        //val beta_0_values:ArrayBuffer[Double] = boundaryIndices.map(index => this.y(index) - SVM.innerProduct(this.beta, this.X(index)))
        val beta_0_values: ArrayBuffer[Double] = new ArrayBuffer[Double]
        for (index <- boundaryIndices)
        {
            beta_0_values.append(this.y(index) - SVM.innerProduct(this.beta, this.X(index)))
        }
        if (beta_0_values.length == 0) 
            return beta_0_values
        else
        {
            this.beta_0 = SVM.mean(beta_0_values)
            return beta_0_values
        }
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

    def get_alpha_2_limit(alpha_1: Double, alpha_2: Double, y_1: Int, y_2: Int, C_1: Double, C_2: Double): (Double, Double) = 
    {
        val s: Int = y_1*y_2
        assert(s == 1 || s == -1)
        if (s == -1)
        {
            val k = alpha_1 - alpha_2
            if (y_1 > 0)
            {
                assert(C_1 >= C_2)
                if (C_1 - C_2 <= k && k <= C_1) return (0, C_1 - k)
                else if (0 <= k && k <= C_1 - C_2) return (0, C_2)
                else return (-k, C_2)
            }
            else//y_1 == -1 and y_2 == 1. 
            {
                assert(C_2 >= C_1)
                if (0 <= k && k <= C_1) return (0, C_1 - k) 
                else if (C_1 - C_2 <= k && k <= 0) return (-k, C_1 - k)
                else return (-k, C_2)
                //return get_alpha_2_limit(alpha_2, alpha_1, y_2, y_1, C_2, C_1)
            }
        }
        else
        {
            assert(C_1 == C_2)
            val k = alpha_1 + alpha_2
            return (max(0, k - C_1), min(C_1, k))
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

    def jointOptimize(alpha_1: Double, alpha_2: Double, x_1: ArrayBuffer[Double], x_2: ArrayBuffer[Double], y_1: Int, y_2: Int, C_1: Double, C_2: Double, norm: Double): (Double, Double) = 
    {
        val eta: Double = norm*norm
        val alpha_2_star: Double = alpha_2 + y_2*(SVM.innerProduct(this.beta, x_1) - y_1 - (SVM.innerProduct(this.beta, x_2) - y_2))/eta
        val (left, right) = this.get_alpha_2_limit(alpha_1, alpha_2, y_1, y_2, C_1, C_2)
        val alpha_2_new = SVM.clip(alpha_2_star, left, right)
        val alpha_1_new = alpha_1 - y_1*y_2*(alpha_2_new - alpha_2)
        (alpha_1_new, alpha_2_new)
    }

    def updateEntire():Unit = 
    {
        val random = new Random
        val eps: Double = 1.0e-10
        val eraNumber: Int = 10
        /*var sweepTimes: Int = this.numberOfSamples
        sweepTimes = sweepTimes*(sweepTimes - 1)/2 //min(sweepTimes*(sweepTimes - 1)/2, 10000000)
        val interval: Int = sweepTimes/eraNumber*/
        /*val sweepTimes: Int = this.pairs.length
        val interval: Int = sweepTimes/eraNumber*/
       val sweepTimes: Int = this.numberOfSamples*this.numberOfSamples
       val interval: Int = sweepTimes/eraNumber
        println("sweep time = " + sweepTimes + ", era number = " + eraNumber + ", interval = " + interval)
        this.getBeta
        for (i <- 0 until sweepTimes)
        {
            if (interval == 0)
            {
                println("update entire step index = " + i + ", total = " + sweepTimes)
            }
            else
            {
                if ((i+1)%interval == 0)
                {
                    //println("i = " + (i+1) + ", interval = " + interval + ", remainder = " + (i+1)%interval)
                    println("update entire step index = " + (i+1)/interval + ", total = " + eraNumber)
                }
            }
            /*val pair:(Int, Int) = this.pairs(i)
            val first_index: Int = pair._1
            val second_index: Int = pair._2*/
            val first_index: Int = random.nextInt(this.alpha.length)
            val second_index: Int = random.nextInt(this.alpha.length)
            //val first_index: Int = this.positiveIndices(random.nextInt(this.positiveIndices.length))
            //val second_index: Int = this.negativeIndices(random.nextInt(this.negativeIndices.length))
            breakable
            {
                if (first_index == second_index)
                    break
                else
                {
                    val alpha_1: Double = this.alpha(first_index)
                    val alpha_2: Double = this.alpha(second_index)
                    val x_1: ArrayBuffer[Double] = this.X(first_index)
                    val x_2: ArrayBuffer[Double] = this.X(second_index)
                    val y_1: Int = this.y(first_index)
                    val y_2: Int = this.y(second_index)
                    val C_1: Double = if (y_1 > 0) this.CPositive else this.CNegative
                    val C_2: Double = if (y_2 > 0) this.CPositive else this.CNegative
                    val diff = SVM.norm(SVM.subtract(x_1, x_2))
                    breakable
                    {
                        if (diff < eps) 
                            break
                        else
                        {
                            val(alpha_1_new, alpha_2_new) = this.jointOptimize(alpha_1, alpha_2, x_1, x_2, y_1, y_2, C_1, C_2, diff)
                            //val(alpha_1_new, alpha_2_new) = this.jointOptimize(alpha_1, alpha_2, x_1, x_2, y_1, y_2, diff)
                            this.alpha(first_index) = alpha_1_new
                            this.alpha(second_index) = alpha_2_new
                            val delta_alpha_1 = alpha_1_new - alpha_1
                            val delta_alpha_2 = alpha_2_new - alpha_2
                            //this.beta = SVM.sum(this.beta, SVM.sum(x_1.map(ele => ele*y_1*delta_alpha_1), x_2.map(ele => ele*y_2*delta_alpha_2)))
                            for (betaIndex <- this.beta.indices)
                            {
                                this.beta(betaIndex) = this.beta(betaIndex) + delta_alpha_1*y_1*x_1(betaIndex) + delta_alpha_2*y_2*x_2(betaIndex)
                            }
                        }
                    }
                }
            }
        }
    }

    def hasViolatedKKT(alpha: Double, x: ArrayBuffer[Double], y: Int): Boolean = 
    {
        /*
         KKT Condition: 
         if alpha == 0, then value >= 1;
         if alpha == C, then value <= 1;
         if 0 < alpha < C, then value == 1;
         */
        val C_alpha: Double = if (y > 0) this.CPositive else this.CNegative
        val eps = 1.0e-10
        //assert(abs(alpha) > eps && (this.C - alpha) > eps)
        val value: Double = (SVM.innerProduct(this.beta, x) + this.beta_0)*y
        if (abs(alpha) < eps)
        {
            return !(value >= 1)
        }
        else if (abs(alpha - C_alpha) < eps)
        {
            return !(value <= 1)
        }
        else 
        {
            return !(abs(value - 1) < 1.0e-3)
        }
    }

    def fastUpdate(): Boolean = 
    {
        val random = new Random
        val eps = 1.0e-10
        this.getBeta
        this.getBeta_0
        val possibleIndices: ArrayBuffer[Int] = this.alpha.indices.filter(index => hasViolatedKKT(this.alpha(index), this.X(index), this.y(index))).to[ArrayBuffer]
        /*val possibleIndices: ArrayBuffer[Int] = new ArrayBuffer[Int]
        val possiblePositiveIndices: ArrayBuffer[Int] = new ArrayBuffer[Int]
        val possibleNegativeIndices: ArrayBuffer[Int] = new ArrayBuffer[Int]
        for (index <- this.alpha.indices)
        {
            if (hasViolatedKKT(this.alpha(index), this.X(index), this.y(index)))
            {
                possibleIndices.append(index)
                if (this.y(index) > 0)
                {
                    possiblePositiveIndices.append(index)
                }
                else
                {
                    possibleNegativeIndices.append(index)
                }
            }
        }*/
        println("Number of indices that have violated the KKT condition is " + possibleIndices.length)
        if(possibleIndices.length < 2)
            return false
        //val nonBoundaryIndices:ArrayBuffer[Int] = possibleIndices.filter(index => !this.onBoundary(this.alpha(index)))
        val boundaryIndices: ArrayBuffer[Int] = new ArrayBuffer[Int]
        val nonBoundaryIndices: ArrayBuffer[Int] = new ArrayBuffer[Int]
        for (index <- possibleIndices)
        {
            if (this.onBoundary(this.alpha(index), this.y(index)))
                boundaryIndices.append(index)
            else
                nonBoundaryIndices.append(index)
        }
        println("Number of non boundary indices that have violated the KKT condition is " + nonBoundaryIndices.length)
        /*if (nonBoundaryIndices.length < 2)
            return false
        */
        //println("Number of boundary indices that have violated the KKT condition is " + boundaryIndices.length)
        val pairs = this.shuffle(this.getElementPairs(possibleIndices))
        //val length: Int = nonBoundaryIndices.length
        //val sweepTimes = length*length

        /*val length: Int = possibleIndices.length
        val sweepTimes: Int = length*length*/

        val sweepTimes = pairs.length
        for (i <- 0 until sweepTimes)
        {
            val pair = pairs(i)
            val first_index = pair._1
            val second_index = pair._2

            //val first_index: Int = possiblePositiveIndices(random.nextInt(possiblePositiveIndices.length)) //pair._1
            //val second_index: Int = possibleNegativeIndices(random.nextInt(possibleNegativeIndices.length)) //pair._2

            /*val first_index: Int = possibleIndices(random.nextInt(possibleIndices.length))
            val second_index: Int = possibleIndices(random.nextInt(possibleIndices.length))*/

            //val first_index: Int = boundaryIndices(random.nextInt(boundaryIndices.length))
            //val second_index: Int = boundaryIndices(random.nextInt(boundaryIndices.length))

            //val first_index: Int = nonBoundaryIndices(random.nextInt(nonBoundaryIndices.length))
            //val second_index: Int = nonBoundaryIndices(random.nextInt(nonBoundaryIndices.length))
            breakable
            {
                if (first_index == second_index)
                    break
                else
                {
                    val alpha_1: Double = this.alpha(first_index)
                    val alpha_2: Double = this.alpha(second_index)
                    val x_1: ArrayBuffer[Double] = this.X(first_index)
                    val x_2: ArrayBuffer[Double] = this.X(second_index)
                    val y_1: Int = this.y(first_index)
                    val y_2: Int = this.y(second_index)
                    val C_1: Double = if (y_1 > 0) this.CPositive else this.CNegative
                    val C_2: Double = if (y_2 > 0) this.CPositive else this.CNegative
                    val diff = SVM.norm(SVM.subtract(x_1, x_2))
                    breakable
                    {
                        if (diff < eps) 
                            break
                        else
                        {
                            val (alpha_1_new, alpha_2_new) = this.jointOptimize(alpha_1, alpha_2, x_1, x_2, y_1, y_2, C_1, C_2, diff)
                            this.alpha(first_index) = alpha_1_new
                            this.alpha(second_index) = alpha_2_new
                            val delta_alpha_1 = alpha_1_new - alpha_1
                            val delta_alpha_2 = alpha_2_new - alpha_2
                            //this.beta = SVM.sum(this.beta, SVM.sum(x_1.map(ele => ele*y_1*delta_alpha_1), x_2.map(ele => ele*y_2*delta_alpha_2)))
                            for (i <- this.beta.indices)
                            {
                                this.beta(i) = this.beta(i) + delta_alpha_1*y_1*x_1(i) + delta_alpha_2*y_2*x_2(i)
                            }
                        }
                    }
                }
            }
        }
        return true
    }

    def train(): Unit = 
    {
        val iterationMax: Int = 50
        var counter: Int = 0
        val updateEntireFrequency: Double = 0.02
        val updateEntireInterval: Int = (1.0/updateEntireFrequency).toInt
        val eps: Double = 1.0e-10
        val writer = new PrintWriter(new File("alpha_records.txt"))
        val errorWriter = new PrintWriter(new File("error.txt"))
        val initialParameterFileName = "final_parameters.txt"
        var useInitialParameter: Boolean = false
        if (Files.exists(Paths.get("final_parameters.txt")))
        {
            val lines: Array[String] = Source.fromFile("final_parameters.txt").getLines.to[Array]
            val initial_alpha: ArrayBuffer[Double] = lines(0).split(":")(1).split(",").map(ele => ele.toDouble).to[ArrayBuffer]
            if (this.alpha.length == initial_alpha.length)
            {
                useInitialParameter = true
                for (i <- initial_alpha.indices)
                {
                    this.alpha(i) = initial_alpha(i)
                }
                this.getBeta
                this.getBeta_0
            }
        }
        breakable
        {
            while(counter < iterationMax)
            {
                val alphaOld: ArrayBuffer[Double] = this.alpha.map(ele => ele)
                /*if (counter%updateEntireInterval == 0 || counter == iterationMax-1)
                {
                    this.updateEntire
                }*/
                if (counter == 0) 
                    this.updateEntire
                else
                {
                    var tempCounter: Int = 0
                    val tempCounterMax: Int = 5
                    breakable
                    {
                        while(this.fastUpdate)
                        {
                            tempCounter += 1
                            if (tempCounter >= tempCounterMax) break
                        }
                    }
                }
                //val alphaNew: ArrayBuffer[Double] = this.alpha.map(ele => ele)
                val error: Double = SVM.norm(SVM.subtract(this.alpha, alphaOld))
                println("Counter = " + (counter+1).toString + ", total = " + iterationMax + ", error = " + error)
                errorWriter.write(counter.toString + "  " + error.toString + "\n")
                writer.write("alpha:" + SVM.arrayBufferToString(this.alpha) + "\n")
                if (error < eps) break
                counter += 1
            }
        }
        errorWriter.close()
        writer.close()
        this.getBeta
        var alpha_dot_y: Double = 0.0
        for (i <- this.alpha.indices)
        {
            alpha_dot_y += this.alpha(i)*this.y(i)
        }
        //assert(abs(alpha_dot_y) < eps)
        println("alpha*y = " + alpha_dot_y)
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

    def anneal(increaseRatio: Double, upperLimit: Double): Unit = 
    {
        assert(increaseRatio > 1.0)
        val parameterFileName: String = "final_parameters.txt"
        if (Files.exists(Paths.get(parameterFileName)))
        {
            ("rm -r " + parameterFileName).!
        }
        var counter: Int = 0
        while(this.C < upperLimit)
        {
            counter += 1
            println("*********************** counter = " + counter + ", C = " + this.C + ", increaseRatio = " + increaseRatio + ", upper limit = " + upperLimit + " *******************************")
            this.train()
            ("cp final_parameters.txt " + "parameters_C_" + "%.2f".format(this.C) + ".txt").!
            this.C = this.C*increaseRatio
            this.CNegative = this.C
            this.CPositive = this.CPositive*increaseRatio
        }
    }

    def test(testFileName: String): Unit = 
    {
        assert(Files.exists(Paths.get(testFileName)))
        val (data, labels) = SVM.readFile(testFileName)
        val predictions: ArrayBuffer[Double] = data.map(vector => SVM.innerProduct(this.beta, vector) + this.beta_0)
        SVM.printFile(predictions, labels, "prediction,label", "prediction_label.csv")
        val confusionMatrix:ArrayBuffer[ArrayBuffer[Double]] = SVM.getConfusionMatrix(predictions, labels, "confusion_matrix.txt")
        assert(confusionMatrix.length == 2)
        assert(confusionMatrix(0).length == 2)
    }
}

class SVMGaussKernel(inputFileName: String, C: Double, positiveFactor: Double) extends SVM(inputFileName, C, positiveFactor)
{
    val Q: ArrayBuffer[Double] = new ArrayBuffer[Double]
    var sigma: Double = 0.0
    for (i <- this.alpha.indices)
    {
        Q.append(0.0)
    }

    def this(inputFileName: String, C: Double, positiveFactor: Double, sigma: Double)
    {
        this(inputFileName, C, positiveFactor)
        this.sigma = sigma
    }
    
    def getQ(): Unit = //Q(j) = beta.dot(X(j))
    {
        for (j <- this.Q.indices)
        {
            var s: Double = 0.0
            for (i <- this.alpha.indices)
            {
                s += this.alpha(i)*this.y(i)*SVM.GaussKernel(this.X(i), this.X(j), this.sigma)
            }
            this.Q(j) = s
        }
    }

    override def getBeta_0(): ArrayBuffer[Double] = 
    {
        val boundaryIndices = this.alpha.indices.filter(index => super.onBoundary(this.alpha(index), this.y(index))).to[ArrayBuffer]
        val beta_0_values: ArrayBuffer[Double] = new ArrayBuffer[Double]
        for (index <- boundaryIndices)
        {
            beta_0_values.append(this.y(index) - this.Q(index))
        }
        if (beta_0_values.length == 0)
            return beta_0_values
        else
        {
            this.beta_0 = SVM.mean(beta_0_values)
            return beta_0_values
        }
    }

    def hasViolatedKKT(index: Int): Boolean = 
    {
        /*
         KKT Condition: 
         if alpha == 0, then value >= 1;
         if alpha == C, then value <= 1;
         if 0 < alpha < C, then value == 1;
         */
        val C_alpha: Double = if (this.y(index) > 0) this.CPositive else this.CNegative
        val eps = 1.0e-10
        //assert(abs(alpha) > eps && (this.C - alpha) > eps)
        val value: Double = (this.Q(index) + this.beta_0)*this.y(index)
        if (abs(this.alpha(index)) < eps)
        {
            return !(value >= 1)
        }
        else if (abs(this.alpha(index) - C_alpha) < eps)
        {
            return !(value <= 1)
        }
        else 
        {
            return !(abs(value - 1) < 1.0e-3)
        }
    }

    def updateKernel(first_index: Int, second_index: Int): Unit = 
    {
        if (first_index == second_index) return
        val alpha_1 = this.alpha(first_index)
        val alpha_2 = this.alpha(second_index)
        val x_1: ArrayBuffer[Double] = this.X(first_index)
        val x_2: ArrayBuffer[Double] = this.X(second_index)
        val y_1: Int = this.y(first_index)
        val y_2: Int = this.y(second_index)
        val C_1: Double = if (y_1 > 0) this.CPositive else this.CNegative
        val C_2: Double = if (y_2 > 0) this.CPositive else this.CNegative
        var s: Double = 0.0
        for (i <- x_1.indices)
        {
            s += (x_1(i) - x_2(i))*(x_1(i) - x_2(i))
        }
        val eps = 1.0e-10
        if (s < eps) return
        val eta: Double = 2.0*(1 - exp(-s/(2*this.sigma*this.sigma)))
        val alpha_2_star: Double = alpha_2 + y_2*((this.Q(first_index) - y_1) - (this.Q(second_index) - y_2))/eta
        val (left, right) = this.get_alpha_2_limit(alpha_1, alpha_2, y_1, y_2, C_1, C_2)
        val alpha_2_new = SVM.clip(alpha_2_star, left, right)
        val alpha_1_new = alpha_1 - y_1*y_2*(alpha_2_new - alpha_2)
        this.alpha(first_index) = alpha_1_new
        this.alpha(second_index) = alpha_2_new
        val delta_alpha_1: Double = alpha_1_new - alpha_1
        val delta_alpha_2: Double = alpha_2_new - alpha_2
        for (i <- this.Q.indices)
        {
            this.Q(i) = this.Q(i) + delta_alpha_1*y_1*SVM.GaussKernel(x_1, this.X(i), this.sigma) + delta_alpha_2*y_2*SVM.GaussKernel(x_2, this.X(i), this.sigma)
        }
    }
 
    override def updateEntire(): Unit = 
    {
        val random = new Random
        println("Getting Q ... ")
        this.getQ
        println("Q calculation done. ")
        val sweepTimes: Int = this.alpha.length*this.alpha.length
        val eraNumber: Int = 10
        val interval: Int = sweepTimes/eraNumber
        println("Sweep times = " + sweepTimes + ", eraNumber = "  +eraNumber + ", interval = " + interval)
        for (i <- 0 until sweepTimes)
        {
            if (interval == 0)
            {
                println("i = " + (i+1) + ", total = " + sweepTimes)
            }
            else
            {
                if ((i+1)%interval == 0)
                {
                    println("step index = " + ((i+1)/interval).toString + ", total = " + eraNumber)
                }
            }
            val first_index: Int = random.nextInt(this.alpha.length)
            val second_index: Int = random.nextInt(this.alpha.length)
            this.updateKernel(first_index, second_index)
        }
    }

    override def fastUpdate(): Boolean = 
    {
        val random = new Random
        val eps = 1.0e-10
        this.getQ
        this.getBeta_0
        val possibleIndices: ArrayBuffer[Int] = this.alpha.indices.filter(index => this.hasViolatedKKT(index)).to[ArrayBuffer]
        println("Number of indices that violated the KKT condition: " + possibleIndices.length)
        if (possibleIndices.length < 2) return false
        val boundaryIndices: ArrayBuffer[Int] = possibleIndices.filter(index => this.onBoundary(this.alpha(index), this.y(index))).to[ArrayBuffer]
        println("Number of non-boundary indices that have violated the KKT condition is " + (possibleIndices.length - boundaryIndices.length))
        val pairs = this.shuffle(this.getElementPairs(possibleIndices))

        val sweepTimes = pairs.length
        for (i <- 0 until sweepTimes)
        {
            val pair = pairs(i)
            val first_index = pair._1
            val second_index = pair._2
            this.updateKernel(first_index, second_index)
        }
        return true
    }

    override def train(): Unit =
    {
        val iterationMax: Int = 50
        var counter: Int = 0
        /*val updateEntireFrequency: Double = 0.02
        val updateEntireInterval: Int = (1.0/updateEntireFrequency).toInt*/
        val eps: Double = 1.0e-10
        val writer = new PrintWriter(new File("alpha_records.txt"))
        val errorWriter = new PrintWriter(new File("error.txt"))
        val initialParameterFileName = "final_parameters.txt"
        var useInitialParameter: Boolean = false
        if (Files.exists(Paths.get("final_parameters.txt")))
        {
            val lines: Array[String] = Source.fromFile("final_parameters.txt").getLines.to[Array]
            val initial_alpha: ArrayBuffer[Double] = lines(0).split(":")(1).split(",").map(ele => ele.toDouble).to[ArrayBuffer]
            if (this.alpha.length == initial_alpha.length)
            {
                useInitialParameter = true
                for (i <- initial_alpha.indices)
                {
                    this.alpha(i) = initial_alpha(i)
                }
                this.getBeta
                this.getBeta_0
            }
        }
        breakable
        {
            while(counter < iterationMax)
            {
                val alphaOld: ArrayBuffer[Double] = this.alpha.map(ele => ele)
                if (counter == 0)
                    this.updateEntire
                else
                {
                    var tempCounter: Int = 0
                    val tempCounterMax: Int = 5
                    breakable
                    {
                        while(this.fastUpdate)
                        {
                            tempCounter += 1
                            if (tempCounter >= tempCounterMax) break
                        }
                    }
                }
                val error: Double = SVM.norm(SVM.subtract(this.alpha, alphaOld))
                println("Counter = " + (counter+1).toString + ", total = " + iterationMax + ", error = " + error)
                errorWriter.write(counter.toString + "  " + error.toString + "\n")
                writer.write("alpha:" + SVM.arrayBufferToString(this.alpha) + "\n")
                if (error < eps) break
                counter += 1
            }
        }
        errorWriter.close()
        writer.close()
        this.getQ
        var alpha_dot_y: Double = 0.0
        for (i <- this.alpha.indices)
        {
            alpha_dot_y += this.alpha(i)*this.y(i)
        }
        //assert(abs(alpha_dot_y) < eps)
        println("alpha*y = " + alpha_dot_y)
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

    def predict(vector: ArrayBuffer[Double]): Double = 
    {
        var s: Double = 0.0
        for (i <- this.alpha.indices)
        {
            s += this.alpha(i)*this.y(i)*SVM.GaussKernel(this.X(i), vector, this.sigma)
        }
        s + this.beta_0
    }

    override def test(testFileName: String): Unit = 
    {
        assert(Files.exists(Paths.get(testFileName)))
        val (data, labels) = SVM.readFile(testFileName)
        val predictions: ArrayBuffer[Double] = data.map(vector => this.predict(vector))
        SVM.printFile(predictions, labels, "prediction,label", "prediction_label.csv")
        val confusionMatrix:ArrayBuffer[ArrayBuffer[Double]] = SVM.getConfusionMatrix(predictions, labels, "confusion_matrix.txt")
        assert(confusionMatrix.length == 2)
        assert(confusionMatrix(0).length == 2)
    }
}

object SVM
{    
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
        var (header:String, lines:ArrayBuffer[String]) = SVM.getLines(inputFileName)
        val data: ArrayBuffer[ArrayBuffer[Double]] = lines.map(line => line.split(",").dropRight(1).map(ele => ele.toDouble).to[ArrayBuffer])
        val label: ArrayBuffer[Int] = lines.map(line => line.split(",").last.toInt)
        (data, label)
    }

    def getLabels(inputFileName: String): ArrayBuffer[Int] = 
    {
        val (header: String, lines: ArrayBuffer[String]) = SVM.getLines(inputFileName)
        lines.map(line => line.split(",").last.toInt)
    }

    def arrayToString(array: Array[Double]): String = array.map(ele => ele.toString).reduce((a, b) => a + "," + b)
    //def sum(a: Array[Double], b: Array[Double]): Array[Double] = a.zip(b).map(pair => pair._1 + pair._2)
    //def sum(a: ArrayBuffer[Double], b: ArrayBuffer[Double]): ArrayBuffer[Double] = a.zip(b).map(pair => pair._1 + pair._2)
    def sum(a: ArrayBuffer[Double], b: ArrayBuffer[Double]): ArrayBuffer[Double] = 
    {
        assert(a.length == b.length)
        val result: ArrayBuffer[Double] = new ArrayBuffer[Double]
        for (i <- a.indices)
        {
            result.append(a(i) + b(i))
        }
        result
    }

    def innerProduct(array_1: ArrayBuffer[Double], array_2:ArrayBuffer[Double]):Double = 
    {
        assert(array_1.length == array_2.length)
        //array_1.zip(array_2).map(p => p._1*p._2).reduce(_+_)
        var result: Double = 0
        for (i <- array_1.indices)
        {
            result += array_1(i)*array_2(i)
        }
        result
    }

    def square(a: Double): Double = a*a

    def GaussKernel(a: ArrayBuffer[Double], b: ArrayBuffer[Double], sigma: Double): Double = 
    {
        assert(a.length == b.length)
        var s: Double = 0.0
        for (i <- a.indices)
        {
            s += square(a(i) - b(i))
        }
        exp(-s/(2*sigma*sigma))
    }

    def norm(array: ArrayBuffer[Double]): Double = sqrt(innerProduct(array, array))

    def subtract(array_1: ArrayBuffer[Double], array_2: ArrayBuffer[Double]): ArrayBuffer[Double] = 
    {
        assert(array_1.length == array_2.length)
        //array_1.zip(array_2).map(p => p._1 - p._2)
        val result: ArrayBuffer[Double] = new ArrayBuffer[Double]
        for (i <- array_1.indices)
        {
            result.append(array_1(i) - array_2(i))
        }
        result
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

    def arrayBufferToString[T: Numeric](array:ArrayBuffer[T]): String =
    {
        array.length match
        {
            case 0 => "Empty ArrayBuffer"
            case _ => array.map(ele => ele.toString).reduce((a, b) => a + "," + b)
        }
    }

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

    def getCurve(x: Double, beta: ArrayBuffer[Double], beta_0: Double, mu: Double): Double = 
    {
        assert(beta.length == 2)
        return (mu - beta_0 - x*beta(0))/beta(1)
    }

    def generateBoundary(xLower: Double, xUpper: Double, beta: ArrayBuffer[Double], beta_0: Double, mu: Double, outputFileName: String): Unit = 
    {
        assert(xLower < xUpper)
        assert(beta.length == 2)
        val x: ArrayBuffer[Double] = new ArrayBuffer[Double]
        val y: ArrayBuffer[Double] = new ArrayBuffer[Double]
        val cutNumber: Int = 20
        val delta: Double = (xUpper - xLower)/cutNumber.toDouble
        for (i <- 0 to cutNumber)
        {
            x.append(xLower + i*delta)
            y.append(getCurve(x(i), beta, beta_0, mu))
        }
        val writer = new PrintWriter(new File(outputFileName))
        for (i <- x.indices)
        {
            writer.write(x(i).toString + "  " + y(i).toString + "\n")
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
        writer.close()
        result
    }
    
    def printLines(header: String, lines: ArrayBuffer[String], start: Int, end: Int, outputFileName: String): Unit = //includes start, excludes end. 
    {
        assert(start >= 0 && start < end)
        assert(end <= lines.length)
        val writer = new PrintWriter(new File(outputFileName))
        writer.write(header + "\n")
        for (i <- start until end)
        {
            writer.write(lines(i) + "\n")
        }
        writer.close()
    }

    def split(inputFileName: String, trainRatio: Double, trainFileName: String, testFileName: String): Unit = 
    {
        assert(Files.exists(Paths.get(inputFileName)))
        assert(trainRatio > 0 && trainRatio < 1)
        val (header, lines) = SVM.getLines(inputFileName)
        val trainNumber = (trainRatio*lines.length).toInt
        val trainWriter = new PrintWriter(new File(trainFileName))
        printLines(header, lines, 0, trainNumber, trainFileName)
        printLines(header, lines, trainNumber, lines.length, testFileName)
    }
}

object svm_test
{
    def crossValidation(inputFileName: String, trainRatio: Double, C: Double, positiveFactor: Double, useKernel: Boolean): Unit = 
    {
        assert(Files.exists(Paths.get(inputFileName)))
        assert(trainRatio > 0 && trainRatio < 1)
        assert(C > 0)
        val trainFileName = "train.csv"
        val testFileName = "test.csv"
        println("Splitting the input file ... ")
        SVM.split(inputFileName, trainRatio, trainFileName, testFileName)
        println("Splitting finished. ")
        val labels: ArrayBuffer[Int] = SVM.getLabels(trainFileName)
        val numberOfNegativeSamples: Int = labels.filter(label => label == -1).length
        val numberOfPositiveSamples: Int = labels.filter(label => label == 1).length
        println("Number of positive samples = " + numberOfPositiveSamples)
        println("Number of negative samples = " + numberOfNegativeSamples)
        //val positiveFactor: Double = numberOfNegativeSamples.toDouble/numberOfPositiveSamples.toDouble
        //val positiveFactor: Double = 1.8
        assert(positiveFactor >= 1.0)
        println("positiveFactor = " + positiveFactor)
        println("Creating the classifier ... ")
        if (!useKernel)
        {
            val svm = new SVM(trainFileName, C, positiveFactor)
            /*println("Initial alpha:" + SVM.arrayBufferToString(svm.alpha))*/
            println("Classifier created. ")
            val useTwoDimensionalData: Boolean = (svm.numberOfFeatures == 2)
            println("Training the model ... ")
            svm.train()
            /*val increaseRatio: Double = 3.0
            val upperLimit: Double = 100000.00
            svm.anneal(increaseRatio, upperLimit)*/
            println("Model training finished. ")
            if (useTwoDimensionalData && !useKernel)
            {
                val x: ArrayBuffer[Double] = new ArrayBuffer[Double]
                for (i <- svm.y.indices)
                {
                    x.append(svm.X(i)(0))
                }
                val xLower: Double = x.reduce((a, b) => min(a, b))
                val xUpper: Double = x.reduce((a, b) => max(a, b))
                SVM.generateBoundary(xLower, xUpper, svm.beta, svm.beta_0, -1, "lower_boundary.txt")
                SVM.generateBoundary(xLower, xUpper, svm.beta, svm.beta_0, 0, "boundary.txt")
                SVM.generateBoundary(xLower, xUpper, svm.beta, svm.beta_0, 1, "upper_boundary.txt")
            }
            println("Testing the model ... ")
            svm.test(testFileName)
            println("Model testing finished. ")
        }
        else
        {
            val sigma: Double = 1.0
            val svm = new SVMGaussKernel(trainFileName, C, positiveFactor, sigma)
            /*println("Initial alpha:" + SVM.arrayBufferToString(svm.alpha))*/
            println("Classifier created. ")
            println("Training the model ... ")
            svm.train()
            /*val increaseRatio: Double = 3.0
            val upperLimit: Double = 100000.00
            svm.anneal(increaseRatio, upperLimit)*/
            println("Model training finished. ")
            println("Testing the model ... ")
            svm.test(testFileName)
            println("Model testing finished. ")
        }
    }

    def main(args: Array[String]): Unit = 
    {
        if (args.length != 4)
        {
            println("inputFileName = args(0), trainRatio = args(1), C = args(2), positiveFactor = args(3). ")
            System.exit(-1)
        }

        val t0 = System.nanoTime()
        val inputFileName = args(0)
        val trainRatio = args(1).toDouble
        val C = args(2).toDouble
        val positiveFactor: Double = args(3).toDouble
        assert(C > 0)
        assert(positiveFactor >= 1.0)
        val useKernel: Boolean = false
        crossValidation(inputFileName, trainRatio, C, positiveFactor, useKernel)
        val t1 = System.nanoTime()
        val timeDiff: Double = (t1 - t0)*1.0e-9
        println("Total time used in seconds: " + timeDiff)
        val writer = new PrintWriter(new File("time.txt"))
        writer.write("Total time used in seconds: " + timeDiff.toString + "\n")
        writer.close()
    }
}
