
install.packages("sparklyr")
library(sparklyr)
library(dplyr)
#Then install Spark
spark_install()
spark_install(version = "3.5")
#Check the versions installed
spark_available_versions()
#And set up a local installation of Spark
sc=spark_connect(master = "local")


sparklyr:: spark_install(version = "3.5",hadoop_version = ("2.7"))
system("java -version")
                         
# Install and connect to Spark using sparklyr (recommended)
if (!require("sparklyr")) install.packages("sparklyr")
library(sparklyr)

# Install Spark if it's not already installed.  Adjust version and Hadoop version if needed.
# If you have Spark installed manually, you might skip this step.
spark_install(version = "3.5.0", hadoop_version = "3", verbose = TRUE)  # Set verbose=TRUE for details

# Connect to Spark.  spark_connect() will automatically find your Spark installation
# if SPARK_HOME is set correctly, or if spark_install() was successful.
sc <- spark_connect(version = "3.5.0", hadoop_version = "3")

# Test the connection (optional)
spark_version(sc)

# Example: Create a Spark DataFrame
df <- spark_read_csv(sc, "example_data", path = "path/to/your/data.csv") # Replace with your data path

# Example: Perform a Spark operation (e.g., count rows)
count <- sdf_nrow(df)
print(paste("Number of rows:", count))

# Disconnect from Spark when finished
spark_disconnect(sc)

system("java-version")
sc=spark_connect(master = "local")
library(sparklyr)
library(dplyr)
