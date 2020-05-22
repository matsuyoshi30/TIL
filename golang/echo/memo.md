# echo

https://github.com/labstack/echo

labstack 社が提供している Go の軽量な Web Framework

[ドキュメント](https://echo.labstack.com/)も簡潔で明瞭なのでよい

## 基本的な使用法

Echo instance 生成

```
e := echo.New()
```

Middleware 使用

```
e.Use(middleware.Logger())
e.Use(middleware.Recover())
```

Routing => handler 登録

```
e.GET("/", func(c echo.Context) error {
		return c.String(http.StatusOK, "Hello, World!\n")
})
```

Server 起動

```
e.Logger.Fatal(e.Start(":1323"))
```

## sample

https://github.com/matsuyoshi30/echo-sample

- Go
- echo
- SQLite
- gorp
