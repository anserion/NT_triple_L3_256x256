# NT_triple_L3_256x256
Трехслойная нейронная сеть восстановления зашумленного видеопотока (Three-layer neural network for recovering a noisy video stream)
Видеофайл или статичное изображение средствами VLC преобразуются к размеру 256x256 пикселей (вырезка без потери качества), обесцвечивается и загружается в рецепторное поле трехслойной нейронной сети с потерей качества, устанавливаемой параметром "шум". Рецепторное поле имеет размер 64x64 ячейки (изображение входной поток из размера 256х256 масштабируется к 64х64 простым усреднением блока пикселей). Далее изображение обрабатывается тремя слоями полносвязной нейронной сети и выводится в окно 64х64 ячейки, промасштабированные до размера 512х512 пикселей. Количество нейронов в первом и втором слоях нейронной сети можно задать программно (1-1000). Количество нейронов выходного слоя сети фиксировано и равно 4096 (нейроны выходного слоя сгруппированы в таблицу 64х64). Для обучения сети используется алгоритм обратного распространения ошибки. В качестве эталона используется входное изображение из рецепторного поля (64х64 пикселя).
Для работы необходим установленный VLC-плеер (используется для открытия видео-файлов). В операционной системе Windows VLC обязательно 32-битный.
