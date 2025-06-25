# NetGet

Простой парсер новостей BBC для Telegram канала.

## Описание

NetGet - это Python-скрипт для автоматического получения последних новостей с BBC News. Идеально подходит для автоматизации контента в Telegram каналах.

## Требования

- Python 3.12+
- beautifulsoup4
- requests

## Установка

### Из исходного кода
```bash
git clone https://github.com/ruzen42/news-netget # https://github.com/AnmiTaliDev/news-netget
cd news-netget
pip install -e .
```

### Через Poetry
```bash
poetry install
```

## Использование

### Базовое использование
```python
from netget import parse_news

# Получить новости
news = parse_news("https://www.bbc.com/news")

# Вывести результат
for i, item in enumerate(news, 1):
    print(f"{i}. {item['title']}")
    print(f"Link: {item['link']}\n")
```

### Запуск скрипта
```bash
python src/netget.py
```

## Структура данных

Функция `parse_news()` возвращает список словарей со следующей структурой:

```python
{
    "title": "Заголовок новости",
    "link": "https://www.bbc.com/news/полная-ссылка"
}
```

## Примеры вывода

```
Find: 15
1. Breaking: Major news story title
Link: https://www.bbc.com/news/story-link

2. Another important update
Link: https://www.bbc.com/news/another-story

...
```

## Возможные проблемы

### Пустой результат
Если парсер не находит новости, возможные причины:
- BBC изменили структуру сайта (CSS селекторы)
- Блокировка по IP или User-Agent
- Проблемы с интернет-соединением

### Решение проблем
1. Проверьте интернет-соединение
2. Убедитесь, что BBC.com доступен из вашего региона
3. При изменении структуры сайта - обновите CSS селекторы в коде

## Лицензия

BSD 3-Clause License

## Авторы

- ruzen42 - Main developer

- AnmiTaliDev - pull request author

---

*Этот проект создан для образовательных целей. Соблюдайте Terms of Service целевых сайтов.*