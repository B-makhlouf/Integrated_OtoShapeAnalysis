# =============================================================================
# OTOLITH CLASSIFICATION USING KERAS DEEP LEARNING
# =============================================================================
# Purpose: Train a CNN to classify otolith photos by watershed based on shape
# Author: Your Name
# Date: September 2025
# =============================================================================

# =============================================================================
# SECTION 1: IMPORT LIBRARIES
# =============================================================================

# Core libraries for numerical operations and neural networks
import seaborn as sns
from sklearn.metrics import confusion_matrix, classification_report
import numpy as np                      # For numerical operations and arrays
import tensorflow as tf                 # TensorFlow main library
from tensorflow import keras            # Main deep learning framework
from tensorflow.keras import layers     # Neural network building blocks
import matplotlib.pyplot as plt        # For plotting and visualizing images
from tensorflow.keras.callbacks import EarlyStopping

# Libraries for loading and organizing image files
import os                                           # For file system navigation
# Updated image loading tools
from tensorflow.keras.utils import load_img, img_to_array
# Data splitting tool
from sklearn.model_selection import train_test_split

print("✓ All libraries imported successfully!")

# =============================================================================
# SECTION 2: SET UP DATA PATHS AND PARAMETERS
# =============================================================================

# Path to your otolith image folders
data_path = "/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/ForOutlines/Original"

# Image size parameters - all photos will be resized to this standard size
img_height = 128    # Height in pixels
img_width = 128     # Width in pixels

print(f"✓ Data path set: {data_path}")
print(f"✓ Image dimensions: {img_width} x {img_height} pixels")

# =============================================================================
# SECTION 3: INITIALIZE DATA STORAGE
# =============================================================================

# Create empty lists to store our data
images = []         # Will hold all image pixel data as arrays
labels = []         # Will hold watershed numbers (0, 1, 2, etc.)
class_names = []    # Will hold watershed names ('KK', 'NK', etc.)

print("✓ Data storage initialized")

# =============================================================================
# SECTION 4: LOAD OTOLITH IMAGES FROM FOLDERS (FIXED VERSION)
# =============================================================================

print("\n" + "="*50)
print("LOADING OTOLITH IMAGES")
print("="*50)

# Get list of watershed folders and filter out hidden files
all_folders = os.listdir(data_path)
watershed_folders = [folder for folder in sorted(all_folders)
                     if os.path.isdir(os.path.join(data_path, folder))
                     and not folder.startswith('.')]

print(f"Found valid watershed folders: {watershed_folders}")

# Process each watershed folder with corrected indexing
# enumerate starts at 0!
for class_idx, watershed_folder in enumerate(watershed_folders):

    # Build full path to this watershed folder
    folder_path = os.path.join(data_path, watershed_folder)

    # Add this watershed to our class list
    class_names.append(watershed_folder)
    print(f"\n📁 Processing: {watershed_folder} (Class #{class_idx})")

    # Count images loaded from this watershed
    image_count = 0

    # Load all .jpg images from this watershed folder
    for image_file in os.listdir(folder_path):
        if image_file.lower().endswith('.jpg'):

            # Build complete path to image file
            img_path = os.path.join(folder_path, image_file)

            try:
                # Load and resize image to standard size
                img = load_img(img_path, target_size=(
                    img_height, img_width), color_mode='grayscale')

                # Convert image to numerical array
                img_array = img_to_array(img)

                # Normalize pixel values to 0-1 range (helps neural network learn)
                img_array = img_array / 255.0

                # Store image data and its watershed label
                images.append(img_array)
                labels.append(class_idx)  # This will now be 0 for KK, 1 for NK
                image_count += 1

            except Exception as e:
                print(f"❌ Error loading {image_file}: {e}")

    print(f"   ✓ Loaded {image_count} images from {watershed_folder}")

# =============================================================================
# SECTION 5: DATA LOADING SUMMARY (UPDATED)
# =============================================================================

print("\n" + "="*50)
print("DATA LOADING COMPLETE")
print("="*50)
print(f"Total images loaded: {len(images)}")
print(f"Number of watersheds: {len(class_names)}")
print(f"Watershed classes: {class_names}")

# Show class mapping
print(f"\nClass mapping:")
for idx, name in enumerate(class_names):
    print(f"  Class {idx}: {name}")

# Show the shape of our data
if len(images) > 0:
    sample_image = images[0]
    print(f"\nImage data shape: {sample_image.shape}")
    print(f"  - Width: {sample_image.shape[1]} pixels")
    print(f"  - Height: {sample_image.shape[0]} pixels")
    print(f"  - Color channels: {sample_image.shape[2]} (RGB)")

# Show label distribution
unique_labels, counts = np.unique(labels, return_counts=True)
print(f"\nLabel distribution:")
for label, count in zip(unique_labels, counts):
    print(f"  Class {label} ({class_names[label]}): {count} images")

print("\n🎉 Ready for next step: Converting to arrays and splitting data!")

# =============================================================================
# SECTION 6: PREPARE DATA AND DEFINE INPUT SHAPE
# =============================================================================

print("\n" + "="*50)
print("PREPARING OTOLITH DATA FOR TRAINING")
print("="*50)

# Convert lists to numpy arrays
print("Converting image lists to numpy arrays...")
x_data = np.array(images)
y_data = np.array(labels)

print(f"✓ Images array shape: {x_data.shape}")
print(f"✓ Labels array shape: {y_data.shape}")

# Split into training and test sets
print("\nSplitting data into training and test sets...")
x_train, x_test, y_train, y_test = train_test_split(
    x_data, y_data,
    test_size=0.2,
    random_state=42,
    stratify=y_data
)

print(f"✓ Training set: {x_train.shape[0]} images")
print(f"✓ Test set: {x_test.shape[0]} images")

# Define input shape for the CNN model
input_shape = (128, 128, 1)  # Height, Width, Channels (BW)
print(f"\n✓ Input shape for CNN: {input_shape}")

# Convert labels to categorical
num_classes = len(class_names)
y_train_categorical = keras.utils.to_categorical(y_train, num_classes)
y_test_categorical = keras.utils.to_categorical(y_test, num_classes)

print(f"✓ Number of classes: {num_classes}")
print(f"✓ Training labels shape: {y_train_categorical.shape}")
print(f"✓ Test labels shape: {y_test_categorical.shape}")

# =============================================================================
# SECTION 7: BUILD CNN MODEL FOR OTOLITH CLASSIFICATION
# =============================================================================

print("\n" + "="*50)
print("BUILDING CNN MODEL")
print("="*50)

# Build the CNN model with detailed explanations
model = keras.Sequential([

    # INPUT LAYER
    keras.Input(shape=input_shape),

    # =============================================================================
    # CNN BLOCK 1: FEATURE DETECTION
    # =============================================================================

    # CONVOLUTIONAL LAYER 1
    layers.Conv2D(
        filters=32,
        kernel_size=(3, 3),
        activation="relu"
    ),

    # MAX POOLING LAYER 1
    layers.MaxPooling2D(pool_size=(2, 2)),

    # =============================================================================
    # CNN BLOCK 2: COMPLEX FEATURE DETECTION
    # =============================================================================

    # CONVOLUTIONAL LAYER 2
    layers.Conv2D(
        filters=64,
        kernel_size=(3, 3),
        activation="relu"
    ),

    # MAX POOLING LAYER 2
    layers.MaxPooling2D(pool_size=(2, 2)),

    # =============================================================================
    # DENSE BLOCK: CLASSIFICATION DECISION WITH DROPOUT
    # =============================================================================

    # FLATTEN LAYER
    layers.Flatten(),

    # ADD DROPOUT LAYER - THIS IS THE KEY CHANGE!
    # Dropout randomly "turns off" 50% of neurons during training
    # This prevents the model from memorizing and forces it to learn general patterns
    layers.Dropout(0.5),

    # DENSE (FULLY CONNECTED) LAYER
    layers.Dense(
        num_classes,
        activation="softmax"
    ),
])

print("✓ CNN model built successfully!")

# =============================================================================
# MODEL SUMMARY AND EXPLANATION
# =============================================================================

print("\n" + "="*50)
print("MODEL ARCHITECTURE SUMMARY")
print("="*50)

model.summary()

print("\n" + "="*50)
print("WHAT EACH LAYER DOES FOR OTOLITH CLASSIFICATION")
print("="*50)

print("🔍 INPUT LAYER:")
print("   - Receives otolith photos: 128x128 pixels, RGB color")
print("   - Like showing the computer a high-resolution otolith image")

print("\n🎯 CONV2D LAYER 1 (32 filters):")
print("   - Detects basic features: edges, curves, boundaries")
print("   - Each of 32 filters learns different basic patterns")
print("   - ReLU activation removes negative values (keeps only positive features)")

print("\n📉 MAXPOOLING2D LAYER 1:")
print("   - Reduces image size: 128x128 → 64x64")
print("   - Keeps strongest features, discards weak signals")
print("   - Makes model faster and focuses on important details")

print("\n🎯 CONV2D LAYER 2 (64 filters):")
print("   - Detects complex otolith shapes and patterns")
print("   - Combines basic features into meaningful otolith characteristics")
print("   - 64 filters = can recognize 64 different complex patterns")

print("\n📉 MAXPOOLING2D LAYER 2:")
print("   - Further reduces size: 64x64 → 32x32")
print("   - Now has compact representation of otolith features")

print("\n🔄 FLATTEN LAYER:")
print("   - Converts 2D feature maps to 1D list")
print("   - Prepares data for final classification decision")

print("\n🧠 DENSE LAYER (Softmax):")
print("   - Makes final decision: KK or NK watershed?")
print("   - Softmax ensures probabilities add to 100%")
print("   - Output example: [0.92, 0.08] = 92% confident it's KK")

print("\n" + "="*50)
print("MODEL READY FOR TRAINING!")
print("="*50)

# =============================================================================
# SECTION 8: TRAIN THE CNN MODEL
# =============================================================================

print("\n" + "="*50)
print("TRAINING CNN MODEL ON OTOLITH DATA")
print("="*50)

# =============================================================================
# TRAINING PARAMETERS
# =============================================================================

batch_size = 128    # How many otolith images to process at once
epochs = 15         # How many times to go through the entire dataset

print(f"Training parameters:")
print(f"  📦 Batch size: {batch_size} images at a time")
print(f"  🔄 Epochs: {epochs} complete passes through all data")
print(f"  📊 Total training images: {x_train.shape[0]}")
print(f"  📊 Images per epoch: {x_train.shape[0]} (all training images)")
print(f"  📊 Batches per epoch: {x_train.shape[0] // batch_size + 1}")

# =============================================================================
# MODEL COMPILATION - SETTING UP THE LEARNING PROCESS
# =============================================================================

print(f"\n🔧 Compiling model...")

model.compile(
    loss="categorical_crossentropy",    # How to measure prediction mistakes
    optimizer="adam",                   # How to learn from mistakes
    metrics=["accuracy"]                # What to track during training
)

print("✓ Model compiled successfully!")

# =============================================================================
# EXPLANATION OF COMPILATION PARAMETERS
# =============================================================================

print(f"\n" + "="*30)
print("TRAINING SETUP EXPLAINED")
print("="*30)

print("🎯 LOSS FUNCTION (categorical_crossentropy):")
print("   - Measures how wrong the predictions are")
print("   - Perfect prediction = loss of 0")
print("   - Completely wrong = high loss")
print("   - For otoliths: penalizes saying 'KK' when it's actually 'NK'")

print("\n🧠 OPTIMIZER (adam):")
print("   - Controls how the model learns from mistakes")
print("   - Adam = smart algorithm that adjusts learning speed")
print("   - Like a good teacher: goes slow on hard concepts, fast on easy ones")

print("\n📊 METRICS (accuracy):")
print("   - Tracks percentage of otoliths classified correctly")
print("   - 90% accuracy = correctly identified 9 out of 10 otoliths")

print("\n📦 BATCH SIZE (128):")
print("   - Processes 128 otolith images simultaneously")
print("   - Larger batches = more stable learning but uses more memory")
print("   - Smaller batches = faster but more variable learning")

print("\n🔄 EPOCHS (15):")
print("   - Each epoch = model sees every otolith image once")
print("   - 15 epochs = model will see each otolith 15 times")
print("   - More epochs = more learning, but risk of memorizing instead of understanding")

print("\n📈 VALIDATION SPLIT (0.1 = 10%):")
print("   - Takes 10% of training data to test during training")
print("   - Shows if model is learning general patterns vs memorizing")
print("   - Helps detect overfitting (memorizing instead of learning)")

# =============================================================================
# START TRAINING
# =============================================================================

print(f"\n" + "="*50)
print("STARTING TRAINING PROCESS")
print("="*50)
print("🚀 Training will show progress for each epoch...")
print("📊 Watch for:")
print("   - Loss decreasing (model making fewer mistakes)")
print("   - Accuracy increasing (correctly classifying more otoliths)")
print("   - Validation metrics staying close to training metrics")
print("\n⏱️  Training starting now...\n")

# Set up early stopping
early_stopping = EarlyStopping(
    monitor='val_loss',
    patience=3,
    restore_best_weights=True,
    verbose=1
)

# Train the model
history = model.fit(
    x_train, y_train_categorical,    # Training data: images and watershed labels
    batch_size=batch_size,           # Process 128 images at a time
    epochs=epochs,                   # Go through all data 15 times
    validation_split=0.1,            # Use 10% of data for validation during training
    callbacks=[early_stopping]      # ADD THIS LINE
)

print(f"\n" + "="*50)
print("TRAINING COMPLETE!")
print("="*50)

# =============================================================================
# TRAINING RESULTS SUMMARY
# =============================================================================

# Get final training results
final_train_loss = history.history['loss'][-1]
final_train_acc = history.history['accuracy'][-1]
final_val_loss = history.history['val_loss'][-1]
final_val_acc = history.history['val_accuracy'][-1]

print(f"📊 FINAL TRAINING RESULTS:")
print(f"   Training Accuracy: {final_train_acc:.1%}")
print(f"   Training Loss: {final_train_loss:.4f}")
print(f"   Validation Accuracy: {final_val_acc:.1%}")
print(f"   Validation Loss: {final_val_loss:.4f}")

# Interpret results
print(f"\n🔍 WHAT THIS MEANS:")
if final_val_acc > 0.85:
    print(f"   🎉 Excellent! Model can distinguish KK from NK otoliths very well")
elif final_val_acc > 0.75:
    print(f"   ✅ Good! Model learned meaningful otolith shape differences")
elif final_val_acc > 0.65:
    print(f"   ⚠️  Okay, but could improve with more training or data")
else:
    print(f"   ❌ Poor performance - model may need architecture changes")

# Check for overfitting
acc_diff = final_train_acc - final_val_acc
if acc_diff > 0.1:
    print(f"   ⚠️  Possible overfitting: training much better than validation")
    print(f"   💡 Consider: more data, dropout layers, or early stopping")
else:
    print(f"   ✅ Good generalization: similar training and validation performance")

print(f"\n🎯 Ready for next step: Evaluate on test data and visualize results!")

# =============================================================================
# SECTION 9: EVALUATE MODEL PERFORMANCE
# =============================================================================

print("\n" + "="*50)
print("EVALUATING MODEL PERFORMANCE")
print("="*50)

# =============================================================================
# SUBSECTION 9.1: VISUALIZE TRAINING HISTORY
# =============================================================================

print("📊 Creating training history visualizations...")

# Create figure with subplots for accuracy and loss
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(15, 5))

# Plot 1: Training and Validation Accuracy
epochs_range = range(1, len(history.history["accuracy"]) + 1)
ax1.plot(epochs_range, history.history["accuracy"],
         'b-', label='Training Accuracy', linewidth=2)
ax1.plot(epochs_range, history.history["val_accuracy"],
         'r-', label='Validation Accuracy', linewidth=2)
ax1.set_title('Training and Validation Accuracy',
              fontsize=14, fontweight='bold')
ax1.set_xlabel('Epochs')
ax1.set_ylabel('Accuracy')
ax1.legend()
ax1.grid(True, alpha=0.3)
ax1.set_ylim([0, 1])

# Plot 2: Training and Validation Loss
ax2.plot(epochs_range, history.history["loss"],
         'b-', label='Training Loss', linewidth=2)
ax2.plot(epochs_range, history.history["val_loss"],
         'r-', label='Validation Loss', linewidth=2)
ax2.set_title('Training and Validation Loss', fontsize=14, fontweight='bold')
ax2.set_xlabel('Epochs')
ax2.set_ylabel('Loss')
ax2.legend()
ax2.grid(True, alpha=0.3)

plt.tight_layout()
plt.show()

print("✓ Training history plots created!")

# =============================================================================
# SUBSECTION 9.2: TEST SET EVALUATION
# =============================================================================

print(f"\n📋 Evaluating model on test set...")

# Evaluate model on test data
test_loss, test_accuracy = model.evaluate(
    x_test, y_test_categorical, verbose=0)

print(f"📊 TEST SET RESULTS:")
print(f"   Test Accuracy: {test_accuracy:.1%}")
print(f"   Test Loss: {test_loss:.4f}")

# Compare with training results
print(f"\n🔍 PERFORMANCE COMPARISON:")
print(f"   Training Accuracy: {final_train_acc:.1%}")
print(f"   Validation Accuracy: {final_val_acc:.1%}")
print(f"   Test Accuracy: {test_accuracy:.1%}")

# Check for overfitting
train_test_diff = final_train_acc - test_accuracy
if train_test_diff > 0.1:
    print(
        f"   ⚠️  Significant overfitting detected: {train_test_diff:.1%} difference")
elif train_test_diff > 0.05:
    print(f"   ⚠️  Mild overfitting: {train_test_diff:.1%} difference")
else:
    print(f"   ✅ Good generalization: {train_test_diff:.1%} difference")

# =============================================================================
# SUBSECTION 9.3: DETAILED PREDICTIONS AND CONFUSION MATRIX
# =============================================================================

print(f"\n🎯 Generating detailed predictions...")

# Get predictions on test set
test_predictions = model.predict(x_test, verbose=0)
predicted_classes = np.argmax(test_predictions, axis=1)
true_classes = np.argmax(y_test_categorical, axis=1)

# Create confusion matrix

cm = confusion_matrix(true_classes, predicted_classes)

# Plot confusion matrix
plt.figure(figsize=(8, 6))
sns.heatmap(cm, annot=True, fmt='d', cmap='Blues',
            xticklabels=class_names, yticklabels=class_names,
            cbar_kws={'label': 'Number of Otoliths'})
plt.title('Confusion Matrix: Otolith Classification Results',
          fontsize=14, fontweight='bold')
plt.xlabel('Predicted Watershed')
plt.ylabel('True Watershed')
plt.show()

print("✓ Confusion matrix created!")

# =============================================================================
# SUBSECTION 9.4: CLASSIFICATION REPORT
# =============================================================================

print(f"\n📊 DETAILED CLASSIFICATION REPORT:")
print("="*50)

# Generate classification report
report = classification_report(true_classes, predicted_classes,
                               target_names=class_names, digits=3)
print(report)

# =============================================================================
# SUBSECTION 9.5: ANALYZE PREDICTIONS BY CLASS
# =============================================================================

print(f"\n🔍 ANALYSIS BY WATERSHED:")
print("="*50)

for i, watershed in enumerate(class_names):
    # Get indices for this watershed
    watershed_indices = np.where(true_classes == i)[0]
    watershed_predictions = predicted_classes[watershed_indices]

    # Calculate accuracy for this watershed
    correct_predictions = np.sum(watershed_predictions == i)
    total_predictions = len(watershed_indices)
    watershed_accuracy = correct_predictions / \
        total_predictions if total_predictions > 0 else 0

    print(f"📍 {watershed} Watershed:")
    print(f"   Total test images: {total_predictions}")
    print(f"   Correctly classified: {correct_predictions}")
    print(f"   Accuracy: {watershed_accuracy:.1%}")

    # Show misclassifications
    if total_predictions > correct_predictions:
        misclassified = total_predictions - correct_predictions
        print(f"   Misclassified: {misclassified} images")

        # Show what they were misclassified as
        for j, other_watershed in enumerate(class_names):
            if i != j:
                misclass_count = np.sum(watershed_predictions == j)
                if misclass_count > 0:
                    print(
                        f"     → {misclass_count} classified as {other_watershed}")
    print()

# =============================================================================
# SUBSECTION 9.6: PREDICTION CONFIDENCE ANALYSIS
# =============================================================================

print(f"🎯 PREDICTION CONFIDENCE ANALYSIS:")
print("="*50)

# Calculate prediction confidence (max probability for each prediction)
prediction_confidence = np.max(test_predictions, axis=1)

print(f"Average prediction confidence: {np.mean(prediction_confidence):.1%}")
print(f"Minimum confidence: {np.min(prediction_confidence):.1%}")
print(f"Maximum confidence: {np.max(prediction_confidence):.1%}")

# Plot confidence distribution
plt.figure(figsize=(10, 6))
plt.hist(prediction_confidence, bins=20, alpha=0.7,
         color='skyblue', edgecolor='black')
plt.axvline(np.mean(prediction_confidence), color='red', linestyle='--',
            label=f'Average: {np.mean(prediction_confidence):.1%}')
plt.title('Distribution of Prediction Confidence',
          fontsize=14, fontweight='bold')
plt.xlabel('Confidence (Maximum Probability)')
plt.ylabel('Number of Predictions')
plt.legend()
plt.grid(True, alpha=0.3)
plt.show()

# Identify low confidence predictions
low_confidence_threshold = 0.6
low_confidence_indices = np.where(
    prediction_confidence < low_confidence_threshold)[0]

print(f"\n⚠️  LOW CONFIDENCE PREDICTIONS:")
print(
    f"   Predictions with <{low_confidence_threshold:.0%} confidence: {len(low_confidence_indices)}")
print(
    f"   Percentage of test set: {len(low_confidence_indices)/len(test_predictions):.1%}")

if len(low_confidence_indices) > 0:
    print(f"   These might be:")
    print(f"   • Otoliths with ambiguous shapes")
    print(f"   • Poor quality images")
    print(f"   • Borderline cases between watersheds")

# =============================================================================
# SUBSECTION 9.7: MODEL PERFORMANCE SUMMARY
# =============================================================================

print(f"\n" + "="*50)
print("MODEL PERFORMANCE SUMMARY")
print("="*50)

print(f"🎯 OVERALL PERFORMANCE:")
print(f"   Final Test Accuracy: {test_accuracy:.1%}")

if test_accuracy >= 0.90:
    performance_rating = "EXCELLENT"
    emoji = "🏆"
elif test_accuracy >= 0.80:
    performance_rating = "VERY GOOD"
    emoji = "🥈"
elif test_accuracy >= 0.70:
    performance_rating = "GOOD"
    emoji = "🥉"
elif test_accuracy >= 0.60:
    performance_rating = "FAIR"
    emoji = "⚠️"
else:
    performance_rating = "NEEDS IMPROVEMENT"
    emoji = "❌"

print(f"   Performance Rating: {emoji} {performance_rating}")

print(f"\n🔍 KEY INSIGHTS:")
print(f"   • Model trained for {epochs} epochs")
print(
    f"   • Best validation accuracy: {max(history.history['val_accuracy']):.1%}")
print(f"   • Training-test gap: {train_test_diff:.1%}")
print(f"   • Average confidence: {np.mean(prediction_confidence):.1%}")

print(f"\n💡 RECOMMENDATIONS:")
if test_accuracy < 0.8:
    print(f"   • Consider collecting more training data")
    print(f"   • Try data augmentation (rotation, flipping)")
    print(f"   • Experiment with different model architectures")
    print(f"   • Adjust hyperparameters (learning rate, batch size)")
elif train_test_diff > 0.1:
    print(f"   • Add dropout layers to reduce overfitting")
    print(f"   • Use early stopping")
    print(f"   • Collect more diverse training data")
else:
    print(f"   • Model performance is good!")
    print(f"   • Consider testing on additional watersheds")
    print(f"   • Document model for operational use")

print(f"\n🎉 MODEL EVALUATION COMPLETE!")
print(f"Your otolith classification model is ready for practical use!")

# =============================================================================
# OPTIONAL: SAVE MODEL FOR FUTURE USE
# =============================================================================

print(f"\n💾 SAVING MODEL:")
model_save_path = "otolith_classifier_model.h5"
model.save(model_save_path)
print(f"✓ Model saved as: {model_save_path}")
print(
    f"💡 Load later with: model = keras.models.load_model('{model_save_path}')")
