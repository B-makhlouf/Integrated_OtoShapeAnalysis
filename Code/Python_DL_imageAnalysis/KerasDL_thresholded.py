# =============================================================================
# OTOLITH CLASSIFICATION USING KERAS DEEP LEARNING WITH BINARY THRESHOLDING
# =============================================================================

import seaborn as sns
from sklearn.metrics import confusion_matrix, classification_report
import numpy as np
import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers
import matplotlib.pyplot as plt
import os
from tensorflow.keras.utils import load_img, img_to_array
from sklearn.model_selection import train_test_split
import cv2

print("✓ All libraries imported successfully!")

# =============================================================================
# SETUP
# =============================================================================

data_path = "/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/OtoPhotos/Original"
figures_path = "/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Figures/Deep Learning"
os.makedirs(figures_path, exist_ok=True)

img_height = 128
img_width = 128

images = []
labels = []
class_names = []

# Store some original images for visualization
original_samples = []
binary_samples = []
sample_labels = []

# =============================================================================
# LOAD IMAGES WITH BINARY THRESHOLDING
# =============================================================================

all_folders = os.listdir(data_path)
watershed_folders = [folder for folder in sorted(all_folders)
                     if os.path.isdir(os.path.join(data_path, folder))
                     and not folder.startswith('.')]

print(f"Found valid watershed folders: {watershed_folders}")

for class_idx, watershed_folder in enumerate(watershed_folders):
    folder_path = os.path.join(data_path, watershed_folder)
    class_names.append(watershed_folder)
    print(f"Processing: {watershed_folder} (Class #{class_idx})")

    image_count = 0
    for image_file in os.listdir(folder_path):
        if image_file.lower().endswith('.jpg'):
            img_path = os.path.join(folder_path, image_file)
            try:
                # Load image in grayscale
                img = load_img(img_path, target_size=(
                    img_height, img_width), color_mode='grayscale')
                img_array = img_to_array(img)

                # Store original for visualization (first 2 from each class)
                if len(original_samples) < len(watershed_folders) * 2:
                    if image_count < 2:
                        original_samples.append(img_array.copy())

                # Apply binary thresholding using Otsu's method
                # Convert to uint8 for cv2
                img_uint8 = img_array.squeeze().astype(np.uint8)

                # Otsu's thresholding automatically finds optimal threshold
                _, binary_img = cv2.threshold(img_uint8, 0, 255,
                                              cv2.THRESH_BINARY + cv2.THRESH_OTSU)

                # Convert back to float and normalize to [0, 1]
                binary_img = binary_img.astype(np.float32) / 255.0

                # Reshape to keep the channel dimension
                binary_img = binary_img.reshape(img_height, img_width, 1)

                # Store for visualization
                if len(binary_samples) < len(watershed_folders) * 2:
                    if image_count < 2:
                        binary_samples.append(binary_img.copy())
                        sample_labels.append(watershed_folder)

                images.append(binary_img)
                labels.append(class_idx)
                image_count += 1

            except Exception as e:
                print(f"Error loading {image_file}: {e}")

    print(f"Loaded {image_count} images from {watershed_folder}")

print(f"Total images loaded: {len(images)}")
print(f"Watershed classes: {class_names}")

# =============================================================================
# VISUALIZE BINARY THRESHOLDING
# =============================================================================

n_samples = len(original_samples)
fig, axes = plt.subplots(n_samples, 2, figsize=(8, n_samples * 2))

if n_samples == 1:
    axes = axes.reshape(1, -1)

for i in range(n_samples):
    # Original image
    axes[i, 0].imshow(original_samples[i].squeeze(), cmap='gray')
    axes[i, 0].set_title(f'{sample_labels[i]} - Original', fontweight='bold')
    axes[i, 0].axis('off')

    # Binary thresholded image
    axes[i, 1].imshow(binary_samples[i].squeeze(), cmap='gray')
    axes[i, 1].set_title(f'{sample_labels[i]} - Binary', fontweight='bold')
    axes[i, 1].axis('off')

plt.suptitle('Binary Thresholding Applied to Otolith Images',
             fontsize=14, fontweight='bold', y=1.0)
plt.tight_layout()
plt.savefig(os.path.join(figures_path, 'binary_thresholding_examples.png'),
            dpi=300, bbox_inches='tight')
plt.close()
print("✓ Binary thresholding visualization saved!")

# =============================================================================
# PREPARE DATA
# =============================================================================

x_data = np.array(images)
y_data = np.array(labels)

x_train, x_test, y_train, y_test = train_test_split(
    x_data, y_data, test_size=0.2, random_state=42, stratify=y_data
)

input_shape = (128, 128, 1)
num_classes = len(class_names)
y_train_categorical = keras.utils.to_categorical(y_train, num_classes)
y_test_categorical = keras.utils.to_categorical(y_test, num_classes)

print(f"Training set: {x_train.shape[0]} images")
print(f"Test set: {x_test.shape[0]} images")

# =============================================================================
# BUILD MODEL
# =============================================================================

model = keras.Sequential([
    keras.Input(shape=input_shape),

    layers.Conv2D(filters=32, kernel_size=(3, 3), activation="relu"),
    layers.MaxPooling2D(pool_size=(2, 2)),

    layers.Conv2D(filters=64, kernel_size=(3, 3), activation="relu"),
    layers.MaxPooling2D(pool_size=(2, 2)),

    layers.Conv2D(filters=128, kernel_size=(3, 3), activation="relu"),
    layers.MaxPooling2D(pool_size=(2, 2)),

    layers.Flatten(),
    layers.Dropout(0.5),
    layers.Dense(128, activation="relu"),
    layers.Dropout(0.3),
    layers.Dense(num_classes, activation="softmax"),
])

model.summary()

# =============================================================================
# TRAIN MODEL
# =============================================================================

batch_size = 32
epochs = 30

early_stopping = keras.callbacks.EarlyStopping(
    monitor='val_loss',
    patience=3,
    min_delta=0.01,
    restore_best_weights=True,
    verbose=1
)

model.compile(
    loss="categorical_crossentropy",
    optimizer=keras.optimizers.Adam(learning_rate=0.0005),
    metrics=["accuracy"]
)

print("Training model with binary thresholded images...")

history = model.fit(
    x_train, y_train_categorical,
    batch_size=batch_size,
    epochs=epochs,
    validation_split=0.2,
    callbacks=[early_stopping],
    verbose=1
)

final_train_acc = history.history['accuracy'][-1]
final_val_acc = history.history['val_accuracy'][-1]
print(f"Training Accuracy: {final_train_acc:.1%}")
print(f"Validation Accuracy: {final_val_acc:.1%}")

# =============================================================================
# EVALUATE MODEL
# =============================================================================

# Plot training history
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
plt.savefig(os.path.join(figures_path, 'training_history.png'),
            dpi=300, bbox_inches='tight')
plt.close()

# Test set evaluation
test_loss, test_accuracy = model.evaluate(
    x_test, y_test_categorical, verbose=0)
print(f"Test Accuracy: {test_accuracy:.1%}")

# Confusion matrix
test_predictions = model.predict(x_test, verbose=0)
predicted_classes = np.argmax(test_predictions, axis=1)
true_classes = np.argmax(y_test_categorical, axis=1)

cm = confusion_matrix(true_classes, predicted_classes)
cm_percent = cm.astype('float') / cm.sum(axis=1)[:, np.newaxis] * 100

plt.figure(figsize=(8, 6))
annot = np.array([[f'{count}\n({pct:.1f}%)' for count, pct in zip(row_counts, row_pcts)]
                  for row_counts, row_pcts in zip(cm, cm_percent)])
sns.heatmap(cm, annot=annot, fmt='', cmap='Blues',
            xticklabels=class_names, yticklabels=class_names,
            cbar_kws={'label': 'Number of Otoliths'})
plt.title('Confusion Matrix: Otolith Classification Results',
          fontsize=14, fontweight='bold')
plt.xlabel('Predicted Watershed')
plt.ylabel('True Watershed')
plt.savefig(os.path.join(figures_path, 'confusion_matrix.png'),
            dpi=300, bbox_inches='tight')
plt.close()

# Classification report
print("Classification Report:")
report = classification_report(
    true_classes, predicted_classes, target_names=class_names, digits=3)
print(report)

# Per-class analysis
print("Analysis by watershed:")
for i, watershed in enumerate(class_names):
    watershed_indices = np.where(true_classes == i)[0]
    watershed_predictions = predicted_classes[watershed_indices]
    correct_predictions = np.sum(watershed_predictions == i)
    total_predictions = len(watershed_indices)
    watershed_accuracy = correct_predictions / \
        total_predictions if total_predictions > 0 else 0

    print(f"{watershed}: {correct_predictions}/{total_predictions} ({watershed_accuracy:.1%})")

# Prediction confidence
prediction_confidence = np.max(test_predictions, axis=1)
print(f"Average prediction confidence: {np.mean(prediction_confidence):.1%}")

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
plt.savefig(os.path.join(figures_path, 'prediction_confidence.png'),
            dpi=300, bbox_inches='tight')
plt.close()

# Save model
model_save_path = "otolith_classifier_model_binary.h5"
model.save(model_save_path)
print(f"Model saved as: {model_save_path}")
