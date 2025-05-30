#!/usr/bin/env python3
"""
Temporal CNN Model Training with OpenEO

This script demonstrates how to train a Temporal CNN (TempCNN) model using OpenEO processes.
The example uses deforestation data from Rondonia to train a deep learning model for time series classification.
"""

import openeo
import rpy2.robjects as robjects

# First, we connect to the back-end and authenticate
con = openeo.connect(
    host="http://127.0.0.1:8000",
    auth_type="basic",
    auth_options={"username": "rolf", "password": "123456"}
)

# Access the OpenEO processes
p = con.processes

# Load the RDS file using rpy2
readRDS = robjects.r['readRDS']
data_deforestation_rondonia = readRDS("./filtered_rondonia_data.rds")

# Serialize the data using jsonlite::serializeJSON
serializeJSON = robjects.r['jsonlite::serializeJSON']
serialized_data = serializeJSON(data_deforestation_rondonia)

# Define the Temporal CNN model architecture
tempcnn_model_def = p.mlm_class_tempcnn(
    cnn_layers=[64, 64, 64],
    cnn_kernels=[5, 5, 5],
    cnn_dropout_rates=[0.2, 0.2, 0.2],
    dense_layer_nodes=256,
    dense_layer_dropout_rate=0.5,
    optimizer="adam",
    learning_rate=0.0005,
    epsilon=0.00000001,
    weight_decay=0.000001,
    lr_decay_epochs=1,
    lr_decay_rate=0.95,
    epochs=150,
    batch_size=64,
    seed=42
)

# Fit the model using the training dataset
tempcnn_model_fitted = p.ml_fit(
    model=tempcnn_model_def,
    training_set=str(serialized_data),
    target="label"
)

# Export the trained model
tempcnn_model = p.export_ml_model(
    model=tempcnn_model_fitted,
    name="tempcnn_model_20_05_25",
    folder="openeocraft-models"
)

# Create and start the job
job = con.create_job(
    graph=tempcnn_model,
    title="Train Temp CNN Model",
    description="Training a Temp CNN model and exporting it"
)
job.start()

# Display job information
print(job.describe()) 