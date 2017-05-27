Курс Функционального программирования в МФТИ.

Лектор: Сошников Дмитрий.

Семинарист: Лаптев Андрей.

# 1 Лабароторная
## Задание 1
Составить программу на функциональном языке, которая печатает таблицу значений элементарной функции, вычисленной двумя способами: по формуле Тейлора и с помощью встроенных функций языка программирования. В качестве аргументов таблицы взять точки разбиения отрезка [a,b] на n равных частей (n + 1 точка, включая концы отрезка), находящихся в рекомендованной области хорошей точности формулы Тейлора. 
Вычисления по формуле Тейлора проводить двумя способами: по экономной в сложностном смысле схеме (с сохранением и домножением предыдущего слагаемого) и «в лоб» вычислением n-го коэффициента. Число слагаемых ряда определяется достижением заданной точности вычислений.
Результат должен быть напечатан в виде таблицы примерно следующего вида:

|x    |Первый способ|Кол-во итераций|Второй способ|Кол-во итераций|Значение функции|
|-----|-------------|---------------|-------------|---------------|----------------|
|0.00	|...          |               |0.0          |...            |                |
|0.05 |||0.0008|...||
|...|...||...|...||
|0.50|...||...|...||

## Задание 2
Составить программу на функциональном языке для решения трансцендентных алгебраических уравнений различными численными методами (итераций, Ньютона и половинного деления). Нелинейные уравнения оформить как параметры-функции, разрешив относительно неизвестной величины в случае необходимости. Применить каждую процедуру к решению трех уравнений, заданных тремя строками таблицы, начиная с варианта с заданным номером. Каждое уравнение решать всеми применимыми методами.
Варианты задания содержатся в таблице 2. Ну и, разумеется, нужно подститать количество итераций для каждого метода.

## Примечание
Для того, чтобы удобно посчитать количество итераций, я ввёл тип Result. Ваши функции должны возвращать именно этот тип. На самом деле, он всего лишь кортеж, где первое значение - результат ваших вычислений, а второе - кол-во итераций.
Можете воспользоваться болванкой, я отметил примерную структуру вашей программы и написал функции printTaylor и printSolve, которые выводят результат, и которые вы должны улучшить.

Помните про то, что языки строгие, поэтому используйте приведение типов 
(так как негоже использовать числа с плавающей точкой там, где нужны только целые)
Для F# это будет [float](https://msdn.microsoft.com/en-us/library/dd233220.aspx) (ну и не забывать расставлять точки), 
для Haskell - [fromIntegral](http://learnyouahaskell.com/types-and-typeclasses)

# 2 Лабороторная
## Лабораторная №2. Деревья
=======

Задание на третью лабораторную работу достаточное простое, сложность может возникнуть только у программистов 
на хаскелле, где генерация случайных значений немного отличается от привычного System.Random, а на семинарах мы рассматриваем только примеры на языке F#.

### Реализовать:
1. Тип данных для представления JSON-объектов (разбирали на семинаре). Будем ещё его называть деревом, так как по сути
это оно и есть, и задания больше относятся к работе с деревом, нежели с JSON
2. Разбор JSON-строки в объект (разбирали на семинаре)
3. Функцию обработки древовидной структуры в соответствии с вариантом задания
4. Функцию генерации случайного JSON-объекта
5. Сериализацию объекта обратно в строку

#### FSharp
Для получения последовательности случайных чисел можно использовать генератор случайных чисел System.Random
#### Haskell
Для получения последовательности случайных чисел можно использовать генератор псевдослучайных чисел 
типа `StdGen` и функцию `random`, или функции получения случайных значений из внешнего мира `randomIO`, `randomRIO`
и т.д.
В первом случае сложность может возникнуть в правильной передаче нового состояния вашего генератора
дальнейшим вычислениям. В этом случае удобно обернуть генерирующие функции в монаду `State`, маленький пример есть в шаблоне.
Во втором случае придётся работать внутри монады IO, что может быть неприятно.
Для многих заданий полезно причислить тип JSON к классу типов `Monoid`

### Тесты
Нужно будет написать тесты, в которых проверить работу программы на нескольких
модельных примерах, показывающих правильность реализованного алгоритма, а также на
случайно сгенерированном дереве.

### Определения
JSON-объекты представлены следующими типами (json.org):
- объект, содержащий пары ключ - значение (другой JSON-объект)
- массив из JSON-объектов
- притивные типы: null, number, string, true, false

Глубиной вершины дерева называется длина пути в эту вершину из корня. Глубиной (высотой)
дерева называется максимальная глубина его вершин. Листом или терминальной вершиной
дерева называется вершина, не имеющая поддеревьев. Степенью вершины называется число
исходящих из неё ветвей. Степенью дерева называется максимальная степень его вершин.
Шириной уровня дерева называется число вершин на данной глубине. Шириной дерева
называется максимальная ширина по всем уровням. Подобие деревьев отличается от равенства
возможным несовпадением значений данных, хранящихся в узлах.

Вариант 20) Добавить произвольный примитивный элемент в дерево, его позиция задаётся списком ключей
