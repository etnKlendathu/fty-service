#pragma once

#include <fty/message-bus.h>
#include <iostream>
#include <malamute.h>
#include <pack/pack.h>

#define EAT(x)
#define REM(x)   x
#define STRIP(x) EAT x
#define PAIR(x)  REM x
#define EMPTY(x)

#define NARGS_SEQ(_1, _2, _3, _4, _5, _6, _7, _8, N, ...) N
#define NARGS(...)                                        NARGS_SEQ(__VA_ARGS__, 8, 7, 6, 5, 4, 3, 2, 1, 0)

#define PRIMITIVE_CAT(x, y) x##y
#define CAT(x, y)           PRIMITIVE_CAT(x, y)

#define APPLY(macro, ...)                          CAT(APPLY_, NARGS(__VA_ARGS__))(macro, __VA_ARGS__)
#define APPLY_1(m, x1)                             m(x1)
#define APPLY_2(m, x1, x2)                         m(x1), m(x2)
#define APPLY_3(m, x1, x2, x3)                     m(x1), m(x2), m(x3)
#define APPLY_4(m, x1, x2, x3, x4)                 m(x1), m(x2), m(x3), m(x4)
#define APPLY_5(m, x1, x2, x3, x4, x5)             m(x1), m(x2), m(x3), m(x4), m(x5)
#define APPLY_6(m, x1, x2, x3, x4, x5, x6)         m(x1), m(x2), m(x3), m(x4), m(x5), m(x6)
#define APPLY_7(m, x1, x2, x3, x4, x5, x6, x7)     m(x1), m(x2), m(x3), m(x4), m(x5), m(x6), m(x7)
#define APPLY_8(m, x1, x2, x3, x4, x5, x6, x7, x8) m(x1), m(x2), m(x3), m(x4), m(x5), m(x6), m(x7), m(x8)

#define METHOD(name, signature) typename ServImplT::template type<signature> name

#define ASYNC(name, ...) typename ServImplT::template type<signature> name

#define SUBJECT(name)                                                                                                                      \
    {                                                                                                                                      \
        name                                                                                                                               \
    }

#define CLIENT_NAME(name)  static constexpr const char* ClientName = name
#define SERVER_NAME(name)  static constexpr const char* ServerName = name
#define CHANNEL_NAME(name) static constexpr const char* ChannelName = name
#define MOVE(fld)          fld = std::move(other.fld);

#define SERVICE_META(name, ...)                                                                                                            \
    friend class Server;                                                                                                                   \
    friend class Client;                                                                                                                   \
    name(const std::string& actorName, const std::string& targName, const std::string& channel, std::unique_ptr<ServImplT>&& impl)         \
        : ServiceBase(actorName, targName, channel, std::move(impl))                                                                       \
    {                                                                                                                                      \
        for (auto* it : methods()) {                                                                                                       \
            it->init(this);                                                                                                                \
        }                                                                                                                                  \
        m_impl->setMethods(methods());                                                                                                     \
    }                                                                                                                                      \
    static fty::Expected<name<ServImplT>> create(const std::string& endpoint)                                                              \
    {                                                                                                                                      \
        std::string actorName = std::is_same_v<Client, ServImplT> ? ClientName : ServerName;                                               \
        std::string targName  = std::is_same_v<Client, ServImplT> ? ServerName : ClientName;                                               \
        if (auto bus = fty::MessageBus::create(actorName, endpoint)) {                                                                     \
            name<ServImplT>                                                                                                                \
                it(actorName, targName, ChannelName, std::unique_ptr<ServImplT>(new ServImplT(actorName, ChannelName, std::move(*bus))));  \
            return fty::Expected<name<ServImplT>>(std::move(it));                                                                          \
        } else {                                                                                                                           \
            return fty::unexpected(bus.error());                                                                                           \
        }                                                                                                                                  \
    }                                                                                                                                      \
    std::vector<InvokeBase*> methods()                                                                                                     \
    {                                                                                                                                      \
        return std::apply(                                                                                                                 \
            [](auto&... elems) {                                                                                                           \
                return std::vector<InvokeBase*>{&elems...};                                                                                \
            },                                                                                                                             \
            std::forward_as_tuple(__VA_ARGS__));                                                                                           \
    }                                                                                                                                      \
    name(name&& other)                                                                                                                     \
        : ServiceBase(std::move(other))                                                                                                    \
    {                                                                                                                                      \
        APPLY(MOVE, __VA_ARGS__)                                                                                                           \
        for (auto* it : methods()) {                                                                                                       \
            it->init(this);                                                                                                                \
        }                                                                                                                                  \
        m_impl->setMethods(methods());                                                                                                     \
    }                                                                                                                                      \
    name& operator=(name&& other)                                                                                                          \
    {                                                                                                                                      \
        APPLY(MOVE, __VA_ARGS__)                                                                                                           \
        ServiceBase::operator=(std::move(other));                                                                                          \
        for (auto* it : methods()) {                                                                                                       \
            it->init(this);                                                                                                                \
        }                                                                                                                                  \
        m_impl->setMethods(methods());                                                                                                     \
    }


#define SERVICE(name)                                                                                                                      \
    template <typename ServImplT>                                                                                                          \
    struct name : public ServiceBase

// =========================================================================================================================================

struct InvokeBase;
struct ComImpl
{
    virtual ~ComImpl() = default;

    virtual fty::MessageBus& bus()                                               = 0;
    virtual void             setMethods(const std::vector<InvokeBase*>& methods) = 0;
};

// =========================================================================================================================================

struct ServiceBase;
struct InvokeBase
{
    virtual ~InvokeBase()                                      = default;
    InvokeBase()                                               = default;
    virtual const std::string& name() const                    = 0;
    virtual void               init(ServiceBase* base)         = 0;
    virtual void               invoke(const fty::Message& msg) = 0;
};

// =========================================================================================================================================

struct ServiceBase
{
    ServiceBase(const std::string& actorName, const std::string& sendToName, const std::string& channel, std::unique_ptr<ComImpl>&& impl)
        : m_actorName(actorName)
        , m_sendToName(sendToName)
        , m_channel(channel)
        , m_impl(std::move(impl))
    {
    }

    virtual ~ServiceBase() = default;

    // ServiceBase(const ServiceBase&) = default;
    ServiceBase(ServiceBase&&) = default;
    // ServiceBase& operator=(const ServiceBase&) = default;
    ServiceBase& operator=(ServiceBase&&) = default;

    fty::MessageBus& bus()
    {
        return m_impl->bus();
    }

    std::string              m_actorName;
    std::string              m_sendToName;
    std::string              m_channel;
    std::unique_ptr<ComImpl> m_impl;
};

// =========================================================================================================================================

class Client : public ComImpl
{
public:
    template <typename Signature>
    struct Invoker;

    template <typename R, typename... Args>
    struct Invoker<R(Args...)>
    {
        using ResultType = R;

        Invoker(const std::string& name)
            : m_name(name)
        {
        }

        // Invoker(std::function<R(Args...)>&& fnc);
        fty::Expected<R> call(const Args&... args)
        {
            fty::Message msg;

            msg.meta.to      = m_base->m_sendToName;
            msg.meta.from    = m_base->m_actorName;
            msg.meta.replyTo = m_base->m_actorName;
            msg.meta.subject = m_name;

            pack(msg, args...);
            auto ret = m_base->bus().send(m_base->m_channel, msg);
            if (!ret) {
                return fty::unexpected(ret.error());
            }

            R result;
            pack::json::deserialize(ret->userData[0], result);
            return result;
        }

        template <typename Arg, typename... TArgs>
        void pack(fty::Message& msg, const Arg& arg, const TArgs&... args)
        {
            pack(msg, arg);
            pack(msg, args...);
        }

        template <typename Arg>
        void pack(fty::Message& msg, const Arg& arg)
        {
            if constexpr (std::is_base_of_v<pack::Attribute, Arg>) {
                msg.userData.append(*pack::json::serialize(arg));
            } else {
                msg.userData.append(fty::convert<std::string>(arg));
            }
        }

        std::string  m_name;
        ServiceBase* m_base = nullptr;
    };

    template <typename Signature>
    class Event : public InvokeBase
    {
    public:
        Event(const std::string& name)
            : inv(name)
        {
        }

        template <typename... Args>
        typename fty::Expected<typename Invoker<Signature>::ResultType> operator()(const Args&... args)
        {
            return inv.call(args...);
        }

        const std::string& name() const override
        {
            return inv.m_name;
        }

        void init(ServiceBase* base) override
        {
            inv.m_base = base;
        }

        void invoke(const fty::Message&) override
        {
        }

    private:
        Invoker<Signature> inv;
    };

    template <typename Signature>
    using type = Event<Signature>;

    Client(const std::string& actorName, const std::string& channelName, fty::MessageBus&& bus)
        : m_actorName(actorName)
        , m_channel(channelName)
        , m_bus(std::move(bus))
    {
    }

    fty::MessageBus& bus() override
    {
        return m_bus;
    }

    void setMethods(const std::vector<InvokeBase*>&) override
    {
    }

    std::string     m_actorName;
    std::string     m_channel;
    fty::MessageBus m_bus;
};

// =========================================================================================================================================

class Server : public ComImpl
{
public:
    template <typename Signature>
    struct Invoker;

    template <typename R, typename... Args>
    struct Invoker<R(Args...)>
    {
        using ResultType = R;

        Invoker(const std::string& name)
            : m_name(name)
        {
        }

        template <typename Func, typename Cls>
        void connect(Func&& fnc, Cls* cls)
        {
            connect([f = std::move(fnc), c = cls](const Args&... args) -> R {
                return std::invoke(f, *c, args...);
            });
        }

        template <typename Func>
        void connect(Func&& fnc)
        {
            m_func = std::move(fnc);
        }

        void invoke(const fty::Message& msg)
        {
            auto         res = std::apply(m_func, vectorToTuple<sizeof...(Args)>(msg.userData));
            fty::Message responce;
            pack(responce, res);
            std::cerr << fmt::format("req: {}", msg) << std::endl;
            std::cerr << fmt::format("resp: {}", responce) << std::endl;
            auto ret = m_base->bus().reply(m_base->m_channel, msg, responce);
        }

        template <typename T>
        std::decay_t<T> convert(const std::string& val)
        {
            if constexpr (std::is_base_of_v<pack::Attribute, std::decay_t<T>>) {
                std::decay_t<T> node;
                pack::json::deserialize(val, node);
                return node;
            } else if constexpr (!std::is_base_of_v<pack::Attribute, std::decay_t<T>>) {
                return fty::convert<T>(val);
            }
            return std::decay_t<T>{};
        }

        template <std::size_t... Indices>
        auto vectorToTupleHelper(const pack::StringList& v, std::index_sequence<Indices...>)
        {
            return std::make_tuple(convert<std::tuple_element_t<Indices, std::tuple<Args...>>>(v[Indices])...);
        }

        template <std::size_t N>
        auto vectorToTuple(const pack::StringList& v)
        {
            assert(std::size_t(v.size()) >= N);
            return vectorToTupleHelper(v, std::make_index_sequence<N>());
        }

        template <typename Arg>
        void pack(fty::Message& msg, const Arg& arg)
        {
            if constexpr (std::is_base_of_v<pack::Attribute, Arg>) {
                msg.userData.append(*pack::json::serialize(arg));
            } else {
                msg.userData.append(fty::convert<std::string>(arg));
            }
        }


        std::string               m_name;
        ServiceBase*              m_base;
        std::function<R(Args...)> m_func;
    };

    template <typename Signature>
    class Callback : public InvokeBase
    {
    public:
        Callback(const std::string& name)
            : inv(name)
        {
        }

        template <typename Func, typename Cls>
        void connect(Func&& fnc, Cls* cls)
        {
            inv.connect(std::move(fnc), cls);
        }

        template <typename Func>
        void connect(Func&& fnc)
        {
            inv.connect(std::move(fnc));
        }

        const std::string& name() const override
        {
            return inv.m_name;
        }

        void init(ServiceBase* base) override
        {
            inv.m_base = base;
        }

        void invoke(const fty::Message& msg) override
        {
            inv.invoke(msg);
        }

    private:
        Invoker<Signature> inv;
    };

    template <typename Signature>
    using type = Callback<Signature>;

    void process(const fty::Message& msg)
    {
        for (auto* meth : m_methods) {
            if (meth->name() == msg.meta.subject) {
                meth->invoke(msg);
                break;
            }
        }
        std::cerr << "got a message\n";
    }

    Server(const std::string& actorName, const std::string& channelName, fty::MessageBus&& bus)
        : m_actorName(actorName)
        , m_channel(channelName)
        , m_bus(std::move(bus))
    {
        std::cerr << "subscribe " << channelName << std::endl;
        auto ret = m_bus.subscribe(channelName, &Server::process, this);
    }

    void setMethods(const std::vector<InvokeBase*>& methods) override
    {
        m_methods = methods;
    }

    fty::MessageBus& bus() override
    {
        return m_bus;
    }

private:
    std::string              m_actorName;
    std::string              m_channel;
    fty::MessageBus          m_bus;
    std::vector<InvokeBase*> m_methods;
};

