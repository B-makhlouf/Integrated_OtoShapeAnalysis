# =============================================================================
# OTOLITH CLASSIFICATION USING KERAS DEEP LEARNING
# =============================================================================

# =============================================================================
# IMPORTS
# =============================================================================

import numpy as np
import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers
import matplotlib.pyplot as plt
import os
from tensorflow.keras.utils import load_img, img_to_array
from sklearn.model_selection import train_test_split
import seaborn as sns
from sklearn.metrics import confusion_matrix, classification_report

# =============================================================================
# CONFIGURATION
# =============================================================================

data_path = "/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/ForOutlines/Original"
img_height = 128
img_width = 128
batch_size = 128
epochs = 15

# =============================================================================
# DATA LOADING - Now using grayscale to focus on shape features
# =============================================================================

images = []
labels = []
class_names = []

all_folders = os.listdir(data_path)
watershed_folders = [folder for folder in sorted(all_folders)
                     if os.path.isdir(os.path.join(data_path, folder))
                     and not folder.startswith('.')]

# Debug print to verify YK is included
print(f"Detected classes: {watershed_folders}")

for class_idx, watershed_folder in enumerate(watershed_folders):
    folder_path = os.path.join(data_path, watershed_folder)
    class_names.append(watershed_folder)

    image_count = 0
    for image_file in os.listdir(folder_path):
        if image_file.lower().endswith('.jpg'):
            img_path = os.path.join(folder_path, image_file)

            try:
                img = load_img(img_path, target_size=(img_height, img_width))
                img_array = img_to_array(img)
                img_array = img_array / 255.0

                images.append(img_array)
                labels.append(class_idx)
                image_count += 1

            except Exception as e:
                print(f"Error loading {img_path}: {e}")
                continue

    print(f"Loaded {image_count} images from class '{watershed_folder}'")

# =============================================================================
# DATA PREPARATION
# =============================================================================

x_data = np.array(images)
y_data = np.array(labels)

print(f"Total images loaded: {len(x_data)}")
print(f"Class distribution: {np.bincount(y_data)}")

x_train, x_test, y_train, y_test = train_test_split(
    x_data, y_data,
    test_size=0.2,
    random_state=42,
    stratify=y_data
)

input_shape = (128, 128, 3)
num_classes = len(class_names)
y_train_categorical = keras.utils.to_categorical(y_train, num_classes)
y_test_categorical = keras.utils.to_categorical(y_test, num_classes)

print(f"Number of classes: {num_classes}")
print(f"Training samples: {len(x_train)}")
print(f"Test samples: {len(x_test)}")

# =============================================================================
# MODEL ARCHITECTURE - Enhanced for 3 classes
# =============================================================================

model = keras.Sequential([
    keras.Input(shape=input_shape),

    # Convolutional Block 1
    layers.Conv2D(filters=32, kernel_size=(3, 3), activation="relu"),
    layers.BatchNormalization(),
    layers.MaxPooling2D(pool_size=(2, 2)),
    layers.Dropout(0.25),

    # Convolutional Block 2
    layers.Conv2D(filters=64, kernel_size=(3, 3), activation="relu"),
    layers.BatchNormalization(),
    layers.MaxPooling2D(pool_size=(2, 2)),
    layers.Dropout(0.25),

    # Convolutional Block 3 - Added for better feature extraction
    layers.Conv2D(filters=128, kernel_size=(3, 3), activation="relu"),
    layers.BatchNormalization(),
    layers.MaxPooling2D(pool_size=(2, 2)),
    layers.Dropout(0.25),

    # Classification Block
    layers.Flatten(),
    layers.Dense(128, activation="relu"),
    layers.BatchNormalization(),
    layers.Dropout(0.5),
    layers.Dense(num_classes, activation="softmax")
])

# =============================================================================
# MODEL COMPILATION AND TRAINING
# =============================================================================

model.compile(
    loss="categorical_crossentropy",
    optimizer="adam",
    metrics=["accuracy"]
)

# Add callbacks for better training
callbacks = [
    keras.callbacks.EarlyStopping(patience=5, restore_best_weights=True),
    keras.callbacks.ReduceLROnPlateau(factor=0.5, patience=3)
]

history = model.fit(
    x_train, y_train_categorical,
    batch_size=batch_size,
    epochs=epochs,
    validation_split=0.1,
    callbacks=callbacks
)

# =============================================================================
# EVALUATION
# =============================================================================

# Training history visualization
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(15, 5))

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

# Test set evaluation
test_loss, test_accuracy = model.evaluate(
    x_test, y_test_categorical, verbose=0)

# Predictions and confusion matrix
test_predictions = model.predict(x_test, verbose=0)
predicted_classes = np.argmax(test_predictions, axis=1)
true_classes = np.argmax(y_test_categorical, axis=1)

cm = confusion_matrix(true_classes, predicted_classes)

plt.figure(figsize=(10, 8))
sns.heatmap(cm, annot=True, fmt='d', cmap='Blues',
            xticklabels=class_names, yticklabels=class_names,
            cbar_kws={'label': 'Number of Otoliths'})
plt.title('Confusion Matrix: Otolith Classification Results',
          fontsize=14, fontweight='bold')
plt.xlabel('Predicted Watershed')
plt.ylabel('True Watershed')
plt.xticks(rotation=45)
plt.yticks(rotation=0)
plt.tight_layout()
plt.show()

# Classification report
report = classification_report(true_classes, predicted_classes,
                               target_names=class_names, digits=3)
print("\nClassification Report:")
print("=" * 60)
print(report)

# Per-class accuracy
class_accuracies = cm.diagonal() / cm.sum(axis=1)
print("\nPer-class Accuracies:")
print("=" * 30)
for i, class_name in enumerate(class_names):
    print(f"{class_name}: {class_accuracies[i]:.3f}")

# Confidence analysis
prediction_confidence = np.max(test_predictions, axis=1)

plt.figure(figsize=(12, 8))

# Overall confidence distribution
plt.subplot(2, 2, 1)
plt.hist(prediction_confidence, bins=20, alpha=0.7,
         color='skyblue', edgecolor='black')
plt.axvline(np.mean(prediction_confidence), color='red', linestyle='--',
            label=f'Average: {np.mean(prediction_confidence):.1%}')
plt.title('Overall Prediction Confidence')
plt.xlabel('Confidence (Maximum Probability)')
plt.ylabel('Number of Predictions')
plt.legend()
plt.grid(True, alpha=0.3)

# Confidence by class
for i, class_name in enumerate(class_names):
    class_mask = true_classes == i
    class_confidence = prediction_confidence[class_mask]

    plt.subplot(2, 2, i+2)
    plt.hist(class_confidence, bins=15, alpha=0.7, edgecolor='black')
    plt.axvline(np.mean(class_confidence), color='red', linestyle='--',
                label=f'Avg: {np.mean(class_confidence):.1%}')
    plt.title(f'Confidence: {class_name}')
    plt.xlabel('Confidence')
    plt.ylabel('Count')
    plt.legend()
    plt.grid(True, alpha=0.3)

plt.tight_layout()
plt.show()

# =============================================================================
# RESULTS SUMMARY
# =============================================================================

print("\n" + "=" * 60)
print("MODEL PERFORMANCE SUMMARY")
print("=" * 60)
print(f"Model Architecture: Enhanced CNN with 3 convolutional blocks")
print(f"Training samples: {len(x_train)}")
print(f"Test samples: {len(x_test)}")
print(f"Number of classes: {num_classes}")
print(f"Class names: {class_names}")
print(f"Final test accuracy: {test_accuracy:.3f}")
print(f"Average prediction confidence: {np.mean(prediction_confidence):.3f}")

# Class balance check
print(f"\nClass distribution in training set:")
train_class_counts = np.bincount(y_train)
for i, (class_name, count) in enumerate(zip(class_names, train_class_counts)):
    percentage = count / len(y_train) * 100
    print(f"  {class_name}: {count} samples ({percentage:.1f}%)")

# =============================================================================
# MODEL SAVING
# =============================================================================

model_filename = "otolith_classifier_3class_model.h5"
model.save(model_filename)
print(f"\nModel saved as: {model_filename}")

# Save class names for future use
np.save("class_names.npy", np.array(class_names))
print("Class names saved as: class_names.npy")
