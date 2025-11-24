import scala.collection.mutable
import scala.io.Source
import scala.util.{Try, Using}
import java.net.URL
import java.io.{File, FileInputStream, InputStream}

// Core Drive Interface (S3 Convention)
trait Drive {
  def listBuckets(): Try[List[String]]
  def listObjects(bucket: String, prefix: String = ""): Try[List[String]]
  def getObject(bucket: String, key: String): Try[Array[Byte]]
  def putObject(bucket: String, key: String, data: Array[Byte]): Try[Unit]
  def deleteObject(bucket: String, key: String): Try[Unit]
  def bucketExists(bucket: String): Try[Boolean]
  def createBucket(bucket: String): Try[Unit]
}

// Built-in Strategy for Google Drive
class GoogleDriveStrategy(url: String) extends Drive {
  override def listBuckets(): Try[List[String]] =
    Try(List("google-drive-root"))

  override def listObjects(bucket: String, prefix: String = ""): Try[List[String]] =
    Try(simulateHttpCall(s"$url/list?prefix=$prefix").split("\n").toList)

  override def getObject(bucket: String, key: String): Try[Array[Byte]] =
    Try(simulateHttpCall(s"$url/get?key=$key").getBytes)

  override def putObject(bucket: String, key: String, data: Array[Byte]): Try[Unit] =
    Try(simulateHttpCall(s"$url/put?key=$key&size=${data.length}"))

  override def deleteObject(bucket: String, key: String): Try[Unit] =
    Try(simulateHttpCall(s"$url/delete?key=$key"))

  override def bucketExists(bucket: String): Try[Boolean] =
    Try(true)

  override def createBucket(bucket: String): Try[Unit] =
    Try(simulateHttpCall(s"$url/bucket?name=$bucket"))

  private def simulateHttpCall(endpoint: String): String = {
    s"[Google Drive] Called: $endpoint"
  }
}

// Built-in Strategy for OneDrive
class OneDriveStrategy(url: String) extends Drive {
  override def listBuckets(): Try[List[String]] =
    Try(List("onedrive-root"))

  override def listObjects(bucket: String, prefix: String = ""): Try[List[String]] =
    Try(simulateHttpCall(s"$url/items?filter=$prefix").split(",").toList)

  override def getObject(bucket: String, key: String): Try[Array[Byte]] =
    Try(simulateHttpCall(s"$url/items/$key/content").getBytes)

  override def putObject(bucket: String, key: String, data: Array[Byte]): Try[Unit] =
    Try(simulateHttpCall(s"$url/items?name=$key&size=${data.length}"))

  override def deleteObject(bucket: String, key: String): Try[Unit] =
    Try(simulateHttpCall(s"$url/items/$key"))

  override def bucketExists(bucket: String): Try[Boolean] =
    Try(true)

  override def createBucket(bucket: String): Try[Unit] =
    Try(simulateHttpCall(s"$url/drives?name=$bucket"))

  private def simulateHttpCall(endpoint: String): String = {
    s"[OneDrive] Called: $endpoint"
  }
}

// Custom HTTP Drive Strategy
class CustomHttpDriveStrategy(url: String) extends Drive {
  override def listBuckets(): Try[List[String]] =
    Try(simulateHttpCall(s"$url/api/v1/buckets").split("\n").toList)

  override def listObjects(bucket: String, prefix: String = ""): Try[List[String]] =
    Try(simulateHttpCall(s"$url/api/v1/buckets/$bucket/objects?prefix=$prefix").split(",").toList)

  override def getObject(bucket: String, key: String): Try[Array[Byte]] =
    Try(simulateHttpCall(s"$url/api/v1/buckets/$bucket/objects/$key").getBytes)

  override def putObject(bucket: String, key: String, data: Array[Byte]): Try[Unit] =
    Try(simulateHttpCall(s"$url/api/v1/buckets/$bucket/objects?key=$key&size=${data.length}"))

  override def deleteObject(bucket: String, key: String): Try[Unit] =
    Try(simulateHttpCall(s"$url/api/v1/buckets/$bucket/objects/$key"))

  override def bucketExists(bucket: String): Try[Boolean] =
    Try(simulateHttpCall(s"$url/api/v1/buckets/$bucket").nonEmpty)

  override def createBucket(bucket: String): Try[Unit] =
    Try(simulateHttpCall(s"$url/api/v1/buckets?name=$bucket"))

  private def simulateHttpCall(endpoint: String): String = {
    s"[Custom HTTP] Called: $endpoint"
  }
}

// Drive Configuration
sealed trait DriveConfig
case class UrlBasedDriveConfig(name: String, url: String, strategy: String) extends DriveConfig
case class CodeBasedDriveConfig(name: String, drive: Drive) extends DriveConfig

// Drive Registry - manages all connected drives
class DriveRegistry {
  private val drives: mutable.Map[String, Drive] = mutable.Map()

  def addDrive(config: DriveConfig): Try[Unit] = Try {
    config match {
      case UrlBasedDriveConfig(name, url, strategy) =>
        val drive = strategy match {
          case "google-drive" => new GoogleDriveStrategy(url)
          case "onedrive" => new OneDriveStrategy(url)
          case "custom" => new CustomHttpDriveStrategy(url)
          case other => throw new IllegalArgumentException(s"Unknown strategy: $other")
        }
        drives(name) = drive
      case CodeBasedDriveConfig(name, drive) =>
        drives(name) = drive
    }
  }

  def getDrive(name: String): Try[Drive] =
    Try(drives(name))
      .recover { case _ => throw new IllegalArgumentException(s"Drive '$name' not found") }

  def listDrives(): List[String] = drives.keys.toList

  def removeDrive(name: String): Try[Unit] =
    Try(drives.remove(name)).map(_ => ())
}

// Main Drive Backend Application
class DriveBackend {
  private val registry = new DriveRegistry()

  def addDrive(config: DriveConfig): Try[Unit] =
    registry.addDrive(config)

  def listBuckets(driveName: String): Try[List[String]] = for {
    drive <- registry.getDrive(driveName)
    buckets <- drive.listBuckets()
  } yield buckets

  def listObjects(driveName: String, bucket: String, prefix: String = ""): Try[List[String]] = for {
    drive <- registry.getDrive(driveName)
    objects <- drive.listObjects(bucket, prefix)
  } yield objects

  def getObject(driveName: String, bucket: String, key: String): Try[Array[Byte]] = for {
    drive <- registry.getDrive(driveName)
    data <- drive.getObject(bucket, key)
  } yield data

  def putObject(driveName: String, bucket: String, key: String, data: Array[Byte]): Try[Unit] = for {
    drive <- registry.getDrive(driveName)
    _ <- drive.putObject(bucket, key, data)
  } yield ()

  def deleteObject(driveName: String, bucket: String, key: String): Try[Unit] = for {
    drive <- registry.getDrive(driveName)
    _ <- drive.deleteObject(bucket, key)
  } yield ()

  def createBucket(driveName: String, bucket: String): Try[Unit] = for {
    drive <- registry.getDrive(driveName)
    _ <- drive.createBucket(bucket)
  } yield ()

  def listConnectedDrives(): List[String] =
    registry.listDrives()

  def removeDrive(name: String): Try[Unit] =
    registry.removeDrive(name)
}

// Example custom implementation
class S3LikeDrive extends Drive {
  private val buckets: mutable.Map[String, mutable.Map[String, Array[Byte]]] = mutable.Map()

  override def listBuckets(): Try[List[String]] =
    Try(buckets.keys.toList)

  override def listObjects(bucket: String, prefix: String = ""): Try[List[String]] = Try {
    buckets.get(bucket)
      .map(_.keys.filter(_.startsWith(prefix)).toList)
      .getOrElse(throw new IllegalArgumentException(s"Bucket '$bucket' not found"))
  }

  override def getObject(bucket: String, key: String): Try[Array[Byte]] = Try {
    buckets(bucket)(key)
  }

  override def putObject(bucket: String, key: String, data: Array[Byte]): Try[Unit] = Try {
    if (!buckets.contains(bucket)) buckets(bucket) = mutable.Map()
    buckets(bucket)(key) = data
  }

  override def deleteObject(bucket: String, key: String): Try[Unit] = Try {
    buckets(bucket).remove(key)
  }

  override def bucketExists(bucket: String): Try[Boolean] =
    Try(buckets.contains(bucket))

  override def createBucket(bucket: String): Try[Unit] = Try {
    buckets(bucket) = mutable.Map()
  }
}

// Demo
object DriveApp extends App {
  val backend = new DriveBackend()

  // Add URL-based drives
  backend.addDrive(UrlBasedDriveConfig("google", "https://google-drive-api.example.com", "google-drive"))
  backend.addDrive(UrlBasedDriveConfig("onedrive", "https://onedrive-api.example.com", "onedrive"))
  backend.addDrive(UrlBasedDriveConfig("custom", "https://custom-drive.example.com", "custom"))

  // Add code-based drive
  val s3Drive = new S3LikeDrive()
  s3Drive.createBucket("my-bucket").get
  s3Drive.putObject("my-bucket", "file.txt", "Hello World".getBytes).get
  backend.addDrive(CodeBasedDriveConfig("s3-like", s3Drive))

  // Demo operations
  println("Connected Drives: " + backend.listConnectedDrives())
  println("\nGoogle Drive Buckets: " + backend.listBuckets("google"))
  println("OneDrive Buckets: " + backend.listBuckets("onedrive"))
  println("S3-like Buckets: " + backend.listBuckets("s3-like"))
  println("S3-like Objects: " + backend.listObjects("s3-like", "my-bucket"))
  println("S3-like Get Object: " + backend.getObject("s3-like", "my-bucket", "file.txt").map(new String(_)))

  backend.removeDrive("google")
  println("\nAfter removing google drive: " + backend.listConnectedDrives())
}


// now make the drive manager to expose the s3 comptaible api for uploading so that we refetence multiple drives as if its one
